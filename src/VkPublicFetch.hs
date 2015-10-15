module VkPublicFetch(runFetching, requestSinglePost) where

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
import           VkPublicParse                (parsePosts, parsePublicInfo, mediaLinksFromHTML)

import           DbFunctions                  (DomPublic, clearPublicIfDirty,
                                               domPublicPublicId, findPublic,
                                               getPublicById, newPublic, openDb,
                                               publicState, savePost,
                                               saveSuspiciousPost,
                                               setPublicFullFetched)

import           Data.String                  as STR
import           Log                          (ScopedLogger, scopedLogger, doLogI, doLogD, doLogE)

logger :: ScopedLogger
logger = scopedLogger "Fetcher"

headerFile :: FilePath
headerFile = "header.txt"

dumpFile :: FilePath
dumpFile = "samples.txt"

maxRetriesNum :: Int
maxRetriesNum = 10

fetchOffsetStep :: Int
fetchOffsetStep = 10

dc :: BS.ByteString -> BS.ByteString
dc = encodeStrictByteString UTF8 . decodeStrictByteString CP1251

convertHtml :: LBS.ByteString -> BS.ByteString
convertHtml bs = dc . LBS.toStrict $ bs

userAgent' :: BS.ByteString
userAgent' = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/31.0.1650.63 Safari/537.36"

retryPolicy :: RetryPolicy
retryPolicy = ((exponentialBackoff 10000) <> (limitRetries maxRetriesNum))

needToRetry :: Int -> (Either HttpException  (Network.HTTP.Conduit.Response LBS.ByteString)) -> IO Bool
needToRetry _  r = return $ either (const True) (const False) r

requestSinglePost :: Int -> Int -> IO (Either String [BS.ByteString])
requestSinglePost ownerId' postId' = do
  logger `doLogD` "requestSinglePost"
  response <- requestSinglePostBS ownerId' postId'
  case response of
    Left e -> do
      logger `doLogE` ("Exception " ++ (show e))
      return $ Left "Couldn't get the response"
    Right _ -> do
      logger `doLogD` "Got response"
      return $ Right $ mediaLinksFromHTML $ responseToPostsBS' response

requestSinglePostBS :: Int -> Int -> IO (Either HttpException (Network.HTTP.Conduit.Response LBS.ByteString))
requestSinglePostBS ownerId' postId' = do
  let url' = "http://vk.com/wall" ++ (show ownerId') ++ "_" ++ (show postId')
  logger `doLogD` ("requestSinglePostBS : url is " ++ url')
  reqq <- parseUrl url'
  let req = urlEncodedBody [] reqq { method = "GET", requestHeaders = [ ("User-Agent", userAgent') ] }
  manager <- newManager tlsManagerSettings
  retrying retryPolicy needToRetry (try (httpLbs req manager))

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
  manager <- newManager tlsManagerSettings
  retrying retryPolicy needToRetry (try (httpLbs req manager))

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

  liftIO $ logger `doLogI` ("Fetching existing public : " ++ show publicId')

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
    Dirty _ -> do
      liftIO $ logger `doLogI` ("Public " ++ (show publicId') ++ " was dirty.")
      liftIO $ logger `doLogI` ("Fetch Result is " ++ (show result))
      case result of
        FetchingSuccess -> liftIO $ setPublicFullFetched publicId'
        _ -> do
          liftIO $ logger `doLogI` ("Unable to fetch all public posts.")
          return ()
    _ -> liftIO $ logger `doLogI` "Public was clean."
  return ()

fetchNewPublic :: BS.ByteString -> ReaderT Config IO ()
fetchNewPublic publicLinkName = do
  config <- ask
  liftIO $ withSocketsDo $ do
    logger `doLogI` ("fetchNewPublic : url = " ++ (url config) ++ ", publicLinkName = " ++ show publicLinkName)
    html <- getWall (dsMode config) (url config)
    savePart config html

    logger `doLogI` ("Public page:\n" ++ (BS.unpack html))
    let publicInfo = parsePublicInfo html
    case publicInfo of
      Just pubInfo -> do
        _ <- newPublic pubInfo publicLinkName
        parsedWall <- parsePosts html
        if (==) 0 $ length $ posts parsedWall
          then logger `doLogI` "Sorry, no data"
          else do maybeDomPublic <- getPublicById $ publicId pubInfo
                  case maybeDomPublic of
                    Just domPublic -> runReaderT (fetchExistingPublic domPublic) config
                    Nothing -> logger `doLogE` "Unable to save public info into database."

      Nothing -> logger `doLogE` "Sorry, can't parse the first page of the public's wall."

runFetching :: String -> String -> IO ()
runFetching url' dsMode' = runReaderT fetchWall Config { url = url', dsMode = read dsMode' }

fetchWall :: ReaderT Config IO ()
fetchWall = do
  config <- ask
  liftIO $ logger `doLogI` ("Starting fetch wall by url : " ++ (url config) ++ ". dsMode = " ++ (show (dsMode config)))

  let publicLinkName = linkNameFromUrl (url config)
  liftIO $ logger `doLogI` ("Public link name = " ++ show publicLinkName)

  liftIO $ openDb

  public <- liftIO $ findPublic $ publicLinkName
  case public of
    Just entity -> do
      liftIO $ logger `doLogI` "Public exists"
      liftIO $ runReaderT (fetchExistingPublic entity) config
    Nothing -> do
      liftIO $ logger `doLogI` "Fetching new public"
      liftIO $ runReaderT (fetchNewPublic publicLinkName) config

getWall :: DataSourceMode -> String -> IO BS.ByteString

getWall DSFromFile _ = fmap BS.pack $ readFile headerFile

getWall _ url' = do
  reqq <- parseUrl url'
  let req = reqq { requestHeaders = [("User-Agent", userAgent')] }
  manager <- newManager tlsManagerSettings
  res <- httpLbs req manager
  return $ convertHtml $ responseBody res

trimLeadingJunk :: BS.ByteString -> BS.ByteString
trimLeadingJunk = DBS.drop 2

responseToBS :: Either HttpException (Response LBS.ByteString) -> LBS.ByteString
responseToBS resp = BSSearch.replace (BS.pack "<!-- -<>->") (BS.pack "") $ convertHtml $ (responseBody $ head $ rights [resp])

responseToPostsBS :: Either HttpException (Response LBS.ByteString) -> BS.ByteString
responseToPostsBS resp = trimLeadingJunk $ LBS.toStrict $ responseToBS resp

responseToPostsBS' :: Either HttpException (Response LBS.ByteString) -> BS.ByteString
responseToPostsBS' resp = LBS.toStrict $ responseToBS resp

getNextPosts :: Int -> Int -> IO BS.ByteString
getNextPosts ownerId' offset' = do
  logger `doLogI` ("Fetching posts by url at offset " ++ show offset')
  result <- retrying retryPolicy needToRetry (requestNextPosts ownerId' offset')
  logger `doLogI` ("Fetching posts by url at offset " ++ (show offset') ++ " done")
  return $ responseToPostsBS result

data FetchState = FetchState { fetchOffset :: FetchOffset, payload :: BS.ByteString }

updateFetchState :: BS.ByteString -> FetchState -> FetchState
updateFetchState newResult oldState = FetchState (fetchOffsetStep + (fetchOffset oldState)) newResult

postUrlSource :: MonadIO m => Config -> OwnerId -> Source (StateT FetchState (m)) BS.ByteString
postUrlSource config ownerId' = do
  fetchState <- ST.get
  liftIO $ logger `doLogI` ("Fetching next part at offset " ++ show (fetchOffset fetchState))
  response <- liftIO $ getNextPosts ownerId' (fetchOffset fetchState)
  liftIO $ logger `doLogI` "Got response"
  ST.modify $ updateFetchState response
  DC.yield response
  postUrlSource config ownerId'

saveResult :: Int -> [Post] -> IO ()
saveResult ownerId' posts' = do
  liftIO $ logger `doLogI` ("Saving " ++ (show $ length posts') ++ " posts to db.")
  mapM_ (savePost ownerId') posts'

saveSuspiciousResult :: Int -> [Post] -> IO ()
saveSuspiciousResult ownerId' posts' = mapM_ (saveSuspiciousPost ownerId') posts'

processFeed' :: MonadIO m => Config -> PublicState -> Sink BS.ByteString m FetchResult
processFeed' config publicState' = do
  let ownerId' = case publicState' of
                   Dirty oid -> oid
                   Clean oid _ _ -> oid

  liftIO $ logger `doLogI` "Waiting for next part"
  mString <- await
  case mString of
    Just string -> do
      parsingResult <- liftIO $ parsePosts $ string
      case parsingResult of
        (ParsingResult posts' _ _) -> do
          let postsCount = length posts'
          liftIO $ logger `doLogI` ("Got " ++ (show postsCount))
          if ((==) 0 $ postsCount)
            then finishFetching
            else case publicState' of
                   Dirty _ -> do
                     liftIO $ logger `doLogI` "processFeed' public is dirty"
                     liftIO $ saveResult ownerId' posts'
                     processFeed' config publicState'
                   Clean _ lastPostId lastPostDate -> do
                     liftIO $ logger `doLogI` "processFeed' public is clean"
                     let (newPosts, otherPosts) = span (\x -> (created x) > lastPostDate) posts'
                       in do
                         liftIO $ saveResult ownerId' newPosts
                         let probablyNewPosts = filter (\x -> (created x) == lastPostDate && (postId x) /= lastPostId) otherPosts
                         case probablyNewPosts of
                           [] -> do liftIO $ logger `doLogI` "All posts already processed"
                                    case newPosts of
                                      [] -> return FetchingSuccess
                                      _ -> processFeed' config publicState'
                           suspiciousPosts -> do
                             liftIO $ saveSuspiciousResult ownerId' suspiciousPosts
                             processFeed' config publicState'

          where finishFetching = do liftIO $ logger `doLogI` "No more posts left"
                                    return FetchingSuccess

        x -> return x
    _ -> return FetchingSuccess
