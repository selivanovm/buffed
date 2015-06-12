module ApiHandler (handlePublicList, handleFeed, handleStartFetching, handleDownloadPost)
where

import           Control.Applicative

import           Snap.Snaplet (Handler)
import           Snap.Snaplet.Heist
import           Snap.Extras.JSON
import           Snap.Core(getParam)
import           Snap.Types(MonadSnap)

import Data.Aeson.QQ
import Data.Aeson.QQ
import Data.Aeson.Types

import           Heist
import qualified Heist.Interpreted as I

import           Application

import Data.Monoid((<>))
import DbFunctions(DomPost, DomPublic, domPostPostId, domPostText, domPostAuthorName, domPostImage, domPostPostId, domPublicLinkName, domPublicPublicId,
                   loadPosts, listPublics, domPublicName, domPublicPublicId, getPublicById, domPublicLinkName, totalPostsCount)
import Buffed(runFetching)
import BuffedData(DataSourceMode(DSNormal))


import Data.Text.Encoding(decodeUtf8)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BSS

import Data.Int (Int64)

import Control.Monad.IO.Class(liftIO)
import Control.Concurrent(forkIO)


s :: String -> String
s p = p

handlePublicList :: Handler App App ()
handlePublicList = do
  publics <- liftIO listPublics
  writeJSON $ map (\public -> [aesonQQ|{ "publicName":#{(decodeUtf8 . domPublicName $ public)}
                                        ,"publicId"  :#{(domPublicPublicId public)}
                                       }|]) publics

handleFeed :: Handler App App ()
handleFeed = do
  pageNumberParam <- getParam "currentPage"
  limitParam <- getParam "limit"
  case (pageNumberParam, limitParam) of
    (Just pageNumberString, Just limitParam') -> do
      let pageNumber' = readInt pageNumberString
      let postsCount = readInt limitParam'

      totalPostsCount' <- liftIO totalPostsCount
      let pagesNumber = totalPostsCount' `div` postsCount
      let pageNumber'' = if pagesNumber  >= pageNumber' then pageNumber' else pagesNumber

      let recordOffset = pageNumber'' * postsCount
      posts <- liftIO $ loadPosts recordOffset postsCount
      let postsJson = map (\post -> [aesonQQ|{ "postId" :#{decodeUtf8 . domPostPostId . fst $ post}
                                              ,"postText":#{(decodeUtf8 $ domPostText $ fst post)}
                                              ,"postUrl" :#{(T.pack("http://vk.com/") <> (decodeUtf8 $ domPublicLinkName $ snd post) <> T.pack("/" ++ (show $ domPostPostId $ fst post)))}
                                              ,"postAuthor":#{(decodeUtf8 $ domPostAuthorName $ fst post)}
                                              ,"postImage":#{(decodeUtf8 $ domPostImage $ fst post)}
                                             }|]) posts

      writeJSON $ [aesonQQ|{ "posts":#{postsJson}, "pagesNumber":#{pagesNumber} }|]

    _ -> writeApiRequestResult NoParam


handleStartFetching :: Handler App App ()
handleStartFetching = do
  param <- getParam "publicId"
  case param of
    Just x -> do
      publicMaybe <- liftIO $ getPublicById $ readInt x
      case publicMaybe of
        Just public -> do
          liftIO $ forkIO $ runFetching [ ("http://vk.com/" ++ (BSS.unpack $ domPublicLinkName public)), show DSNormal ]
          writeApiRequestResult Ok
        _ -> writeApiRequestResult NoPublic
    _ -> writeApiRequestResult NoParam


handleDownloadPost :: Handler App App ()
handleDownloadPost = writeApiRequestResult Ok

readInt = read . BSS.unpack

writeApiRequestResult :: (MonadSnap m, ToJSON a) => a -> m ()
writeApiRequestResult r = writeJSON $ [aesonQQ| { "result": #{r} }|]

data ApiRequestResult = NoParam | NoPublic | Ok
instance ToJSON ApiRequestResult where
  toJSON NoParam  = String "no_param"
  toJSON NoPublic = String "no_public"
  toJSON Ok       = String "ok"
