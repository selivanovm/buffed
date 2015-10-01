module VkPublicFetch(getNextPosts, fetchWall) where

import           Network                      (withSocketsDo)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Header    ()

import qualified Data.ByteString              as DBS (drop, filter)
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.ByteString.Search       as BSSearch
import           Data.Monoid

import           Data.Encoding
import           Data.Encoding.CP1251
import           Data.Encoding.UTF8

import           Data.Either

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State          as ST
import           Control.Monad.Trans.Resource
import           Control.Retry

import           Data.Conduit                 as DC
import           Data.Conduit.Binary          as CB (conduitFile, lines,
                                                     sinkFile, sourceFile)
import qualified Data.Conduit.List            as CL

import           BuffedData
import           VkPublicData
import           VkPublicParse                (parsePosts, parsePublicInfo)

import           Database.Persist
import           DbFunctions                  (DomPublic, clearPublicIfDirty,
                                               domPublicPublicId, findPublic,
                                               getPublicById, newPublic, openDb,
                                               publicState, savePost,
                                               saveSuspiciousPost,
                                               setPublicFullFetched)

import           Data.String                  as STR

import           System.Log.Logger

headerFile :: FilePath
headerFile = "header.txt"

dumpFile :: FilePath
dumpFile = "samples.txt"

maxRetriesNum :: Int
maxRetriesNum = 10

dc :: BS.ByteString -> BS.ByteString
dc = encodeStrictByteString UTF8 . decodeStrictByteString CP1251

convertHtml :: LBS.ByteString -> BS.ByteString
convertHtml bs = dc . LBS.toStrict $ bs

userAgent' :: BS.ByteString
userAgent' = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36"

retryPolicy = ((exponentialBackoff 10000) <> (limitRetries 10))
needToRetry :: Int -> (Either HttpException  (Network.HTTP.Conduit.Response LBS.ByteString)) -> IO Bool
needToRetry _  r = return $ either (const True) (const False) r

requestSinglePost :: Int -> Int -> IO (Either HttpException (Network.HTTP.Conduit.Response LBS.ByteString))
requestSinglePost ownerId' postId' = do
  let url = "http://vk.com/wall-" ++ (show ownerId') ++ "_" ++ (show postId')
  reqq <- parseUrl url
  let req = urlEncodedBody [] reqq { method = "GET", requestHeaders = [ ("User-Agent", userAgent') ] }
  retrying retryPolicy needToRetry (try (withManager $ httpLbs req))

requestNextPosts :: Int -> Int -> IO (Either HttpException (Network.HTTP.Conduit.Response LBS.ByteString))
requestNextPosts ownerId' offset' = do
  reqq <- parseUrl "http://vk.com/al_wall.php"
  let req = urlEncodedBody [ ("act", "get_wall")
                           , ("type", "own")
                           , ("offset", BS.pack $ show offset')
                           , ("owner_id", BS.pack $ show ownerId')
                           , ("al", "1")
                           , ("fixed", "")] $
            reqq { method = "POST", requestHeaders = [ ("User-Agent", userAgent') ] }
  retrying retryPolicy needToRetry (try (withManager $ httpLbs req))

savePart :: Config -> BS.ByteString -> IO ()
savePart config string = case (dsMode config) of
  DSToFile -> do
    let src = yield (string <> "\n")
        sink = sinkFile headerFile
    runResourceT $ src $$ sink
  _ -> return ()

linkNameFromUrl :: String -> BS.ByteString
linkNameFromUrl url' = fromString $ reverse $ takeWhile (/= '/') $ reverse url'

fetchExistingPublic :: DomPublic -> ReaderT Config IO ()
fetchExistingPublic domPublic = do
  config <- ask
  let public = domPublic
      publicId' = domPublicPublicId public

  liftIO $ doLog $ "Fetching existing public : " ++ show publicId'

  publicState' <- liftIO $ publicState domPublic
  liftIO $ clearPublicIfDirty publicState' domPublic
  result <- case (dsMode config) of
    DSFromFile -> do
      runResourceT $ CB.sourceFile dumpFile $= CB.lines $$ (processFeed' config publicState')
    DSNormal -> do
      let postsFromUrlConduit = (postUrlSource config publicId') $$ (processFeed' config publicState')
      fetchedPosts <- runStateT postsFromUrlConduit (FetchState 0 "")
      return $ fst fetchedPosts
    DSToFile -> do
      let removeIntermediateNewlines = (\x -> (DBS.filter (/= "\n") x) <> "\n")
          dumpToFile = CL.map removeIntermediateNewlines $= conduitFile dumpFile
          postsFromUrlWithRecordConduit = (postUrlSource config publicId') $= dumpToFile $$ (processFeed' config publicState')
      stuff <- runResourceT ((runStateT postsFromUrlWithRecordConduit (FetchState 0 "")))
      return $ fst stuff

  case publicState' of
    Dirty oid -> do
      liftIO $ doLog $ "Public " ++ (show publicId') ++ " was dirty."
      liftIO $ doLog $ "Fetch Result is " ++ (show result)
      case result of
        FetchingSuccess -> liftIO $ setPublicFullFetched publicId'
        _ -> do
          liftIO $ doLog $ "Unable to fetch all public posts."
          return ()
    _ -> liftIO $ doLog "Public was clean."
  return ()

fetchNewPublic :: BS.ByteString -> ReaderT Config IO ()
fetchNewPublic publicLinkName = do
  config <- ask
  liftIO $ withSocketsDo $ do
    doLog $ "fetchNewPublic : url = " ++ (url config) ++ ", publicLinkName = " ++ show publicLinkName
    html <- getWall (dsMode config) (url config)
    savePart config html

    debugM "Fetcher" $ ("Public page:\n" ++ (BS.unpack html))
    let publicInfo = parsePublicInfo html
    case publicInfo of
      Just pubInfo -> do
        _ <- newPublic pubInfo publicLinkName
        parsedWall <- parsePosts html
        if (==) 0 $ length $ posts parsedWall
          then infoM "Fetcher" "Sorry, no data"
          else do maybeDomPublic <- getPublicById $ publicId pubInfo
                  case maybeDomPublic of
                    Just domPublic -> runReaderT (fetchExistingPublic domPublic) config
                    Nothing -> errorM "Fetcher" "Unable to save public info into database."

      Nothing -> errorM "Fetcher" "Sorry, can't parse the first page of the public's wall."

fetchWall :: ReaderT Config IO ()
fetchWall = do
  config <- ask
  liftIO $ doLog $ "Starting fetch wall by url : " ++ (url config) ++ ". dsMode = " ++ (show (dsMode config))

  let publicLinkName = linkNameFromUrl (url config)
  liftIO $ doLog $ "Public link name = " ++ show publicLinkName

  liftIO $ openDb

  public <- liftIO $ findPublic $ publicLinkName
  case public of
    Just entity -> do
      liftIO $ doLog "Public exists"
      liftIO $ runReaderT (fetchExistingPublic entity) config
    Nothing -> do
      liftIO $ doLog "Fetching new public"
      liftIO $ runReaderT (fetchNewPublic publicLinkName) config

getWall :: DataSourceMode -> String -> IO BS.ByteString

getWall DSFromFile _ = fmap BS.pack $ readFile headerFile

getWall _ url' = do
  reqq <- parseUrl url'
  let req = reqq { requestHeaders = [("User-Agent", userAgent')] }
  res <- withManager $ httpLbs req
  return $ convertHtml $ responseBody res

getNextPosts :: Int -> Int -> IO BS.ByteString
getNextPosts ownerId' offset' = do
  doLog $ "Fetching posts by url at offset " ++ show offset'
  result <- retrying retryPolicy needToRetry (requestNextPosts ownerId' offset')
  doLog $ "Fetching posts by url at offset " ++ (show offset') ++ " done"
  let prepResp = preparedResponse $ LBS.toStrict $ respString result
  return prepResp
  where
    respString resp = BSSearch.replace (BS.pack "<!-- -<>->") (BS.pack "") $ convertHtml $ (responseBody $ head $ rights [resp])
    preparedResponse resp = DBS.drop 2 $ resp

data FetchState = FetchState { fetchOffset :: FetchOffset, payload :: BS.ByteString }

updateFetchState :: BS.ByteString -> FetchState -> FetchState
updateFetchState newResult oldState = FetchState (10 + (fetchOffset oldState)) newResult

postUrlSource :: MonadIO m => Config -> OwnerId -> Source (StateT FetchState (m)) BS.ByteString
postUrlSource config ownerId' = do
  fetchState <- ST.get
  liftIO $ doLog $ "Fetching next part at offset " ++ show (fetchOffset fetchState)
  response <- liftIO $ getNextPosts ownerId' (fetchOffset fetchState)
  liftIO $ doLog $ "Got response"
  ST.modify $ updateFetchState response
  DC.yield response
  postUrlSource config ownerId'

saveResult :: Int -> [Post] -> IO ()
saveResult ownerId' posts' = do
  liftIO $ doLog $ "Saving " ++ (show $ length posts') ++ " posts to db."
  mapM_ (savePost ownerId') posts'

saveSuspiciousResult :: Int -> [Post] -> IO ()
saveSuspiciousResult ownerId' posts' = do
  mapM_ (saveSuspiciousPost ownerId') posts'

processFeed' :: MonadIO m => Config -> PublicState -> Sink BS.ByteString m FetchResult
processFeed' config publicState' = do
  let ownerId' = case publicState' of
                   Dirty oid -> oid
                   Clean oid _ _ -> oid

  liftIO $ doLog $ "Waiting for next part"
  mString <- await
  case mString of
    Just string -> do
      parsingResult <- liftIO $ parsePosts $ string
      case parsingResult of
        (ParsingResult posts' _ _) -> do
          let postsCount = length posts'
          liftIO $ doLog $ "Got " ++ (show postsCount)
          if ((==) 0 $ postsCount)
            then finishFetching
            else case publicState' of
                   Dirty _ -> do
                     liftIO $ doLog "processFeed' public is dirty"
                     liftIO $ saveResult ownerId' posts'
                     processFeed' config publicState'
                   Clean _ lastPostId lastPostDate -> do
                     liftIO $ doLog "processFeed' public is clean"
                     let (newPosts, otherPosts) = span (\x -> (created x) > lastPostDate) posts'
                       in do
                         liftIO $ saveResult ownerId' newPosts
                         let probablyNewPosts = filter (\x -> (created x) == lastPostDate && (postId x) /= lastPostId) otherPosts
                         case probablyNewPosts of
                           [] -> do liftIO $ doLog "All posts already processed"
                                    case newPosts of
                                      [] -> return FetchingSuccess
                                      _ -> processFeed' config publicState'
                           suspiciousPosts -> do
                             liftIO $ saveSuspiciousResult ownerId' suspiciousPosts
                             processFeed' config publicState'

          where finishFetching = do liftIO $ doLog "No more posts left"
                                    return FetchingSuccess

        x -> return x
    _ -> return FetchingSuccess

doLog = infoM "Fetcher"
