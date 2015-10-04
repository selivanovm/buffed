{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module DbFunctions where

import qualified Data.ByteString.Char8 as BSS

import Data.Text(Text)
import Data.Time

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Control.Monad.Logger

import Database.Persist.Sqlite (runSqlite)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Database.Esqueleto

import System.Log.Logger

import VkPublicData
import BuffedData

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
DomPublic
   publicId         Int
   name             BSS.ByteString
   linkName         BSS.ByteString
   rootFound        Bool
   PublicId         publicId
   PublicLinkName   linkName
   deriving Show

DomUserRef
   name        Text
   accountLink Text
   deriving Show

DomMediaRef
   publicId   Int
   postId     DomPostId
   artist     BSS.ByteString
   title      BSS.ByteString
   deriving Show

DomPost
   publicId   Int
   postId     BSS.ByteString
   created    UTCTime
   image      BSS.ByteString
   authorId   BSS.ByteString
   authorName BSS.ByteString
   signer     DomUserRefId
   text       BSS.ByteString
   visible    Bool
   downloaded Bool
   PostId     postId
   deriving Show
|]

withDb :: (MonadBaseControl IO m, MonadIO m) => SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
withDb = runSqlite "buffed.db"

openDb :: IO ()
openDb = withDb $ do
       runMigration migrateTables

getPublicById :: Int -> IO (Maybe DomPublic)
getPublicById publicId' = withDb $ do
  domPublic <- getBy $ PublicId publicId'
  return $ fmap entityVal $ domPublic

getPost :: Int -> BSS.ByteString -> IO (Maybe DomPost)
getPost publicId' postId' = withDb $ do
  ps <- select $
        from $ \p -> do
        where_ ((p ^. DomPostPublicId ==. (val publicId')) &&. (p ^. DomPostPostId ==. (val postId')))
        return p


  return $ Just $ entityVal $ head ps

getPostOffset :: Int -> BSS.ByteString -> IO (Maybe Int)
getPostOffset publicId' postId' = withDb $ do
  ret <- select $
         from $ \post -> do
           let postDate = from $ \postWithId' -> do
               where_ $ postWithId' ^. DomPostPostId ==. (val postId') &&. postWithId' ^. DomPostPublicId ==. (val publicId')
               limit 1
               return $ postWithId' ^. DomPostCreated
           where_ $ post ^. DomPostPublicId ==. (val publicId') &&. post ^. DomPostCreated >=. sub_select postDate
           return post
  return $ Just $ length ret

findPublic :: BSS.ByteString -> IO (Maybe DomPublic)
findPublic linkName' = withDb $ do
  domPublic <- getBy $ PublicLinkName linkName'
  return $ fmap entityVal $ domPublic

newPublic :: PublicInfo -> BSS.ByteString -> IO (Key DomPublic)
newPublic publicInfo linkName' = withDb $ insert $
                                 DomPublic (publicId publicInfo) (publicName $ publicInfo) linkName' False

deleteAllPostsByPublic :: DomPublic -> IO ()
deleteAllPostsByPublic public = withDb $ do
  let publicIdVal = val $ domPublicPublicId $ public
  liftIO $ infoM "Db" $ "Removing all data for public with id = " ++ (show $ domPublicPublicId $ public)
  delete $
    from $ \t -> where_ (t ^. DomPostPublicId ==. publicIdVal)
  delete $
    from $ \t ->
     where_ (t ^. DomMediaRefPublicId ==. publicIdVal)

publicState :: DomPublic -> IO PublicState
publicState public
  | domPublicRootFound $ public = do
      state' <- withDb $ do
        select $
          from $ \domPost -> do
            where_ (domPost ^. DomPostPublicId ==. (val . domPublicPublicId $ public))
            orderBy [ desc (domPost ^. DomPostCreated) ]
            limit 1
            return $ (domPost ^. DomPostPostId, domPost ^. DomPostCreated)
      case state' of
        [] -> return $ Dirty (domPublicPublicId public)
        ((Value postId', Value postCreated'):_) -> do
            liftIO $ infoM "Db" $ "Clean public. Last post date " ++ (show postCreated')
            return $ Clean (domPublicPublicId public) postId' postCreated'
  | otherwise = do
    liftIO $ infoM "Db" $ "Dirty public"
    return $ Dirty (domPublicPublicId public)

clearPublicIfDirty :: PublicState -> DomPublic -> IO ()
clearPublicIfDirty (Dirty _) public = deleteAllPostsByPublic public
clearPublicIfDirty (Clean _ _ _) _ = return ()

setPublicFullFetched :: Int -> IO ()
setPublicFullFetched publicId' = withDb $ do
  update $ \p -> do
    set p [ DomPublicRootFound =. val True ]
    where_ $ p ^. DomPublicPublicId ==. val publicId'
  liftIO $ infoM "Db" $ "Set public #" ++ (show publicId') ++ " to clean state."

setPostDownloaded :: Int -> BSS.ByteString -> IO ()
setPostDownloaded publicId' postId' = withDb $ do
  update $ \p -> do
    set p [ DomPostDownloaded =. val True ]
    where_ $ p ^. DomPostPublicId ==. val publicId' &&. p ^. DomPostPostId ==. val postId'
  liftIO $ infoM "Db" $ "Set post #" ++ (BSS.unpack postId') ++ "in public #" ++ (show publicId') ++ " to downloaded"

savePost :: Int -> Post -> IO ()
savePost publicId' post = withDb $ do
  postId' <- insert $ DomPost (publicId') (postId $ post) (created post) (BSS.pack "") (authorId post) (authorName  post) (toSqlKey 0) (text post) True False
  _ <- mapM (\mediaRef -> insert $ DomMediaRef publicId' (postId') (artist mediaRef) (title $ mediaRef)) $ media post
  return ()

loadPosts :: Int -> Int -> IO [(DomPost, DomPublic)]
loadPosts recordOffset postsCount = withDb $ do
  ps <- select $
    from $ \(domPost `InnerJoin` domPublic) -> do
      on (domPost ^. DomPostPublicId ==. domPublic ^. DomPublicPublicId)
      orderBy [ desc (domPost ^. DomPostCreated) ]
      where_ $ domPost ^. DomPostDownloaded ==. val False &&. domPost ^. DomPostVisible ==. val True
      offset (fromIntegral recordOffset)
      limit (fromIntegral postsCount)
      return (domPost, domPublic)
  return $ map (\x -> (entityVal (fst x), entityVal (snd x))) ps

totalPostsCount :: IO Int
totalPostsCount = withDb $ do
  cnt' <-
    select $ from $ \domPost -> do
      where_ $ domPost ^. DomPostDownloaded ==. val False &&. domPost ^. DomPostVisible ==. val True
      let cnt = countRows :: SqlExpr (Value Int)
      return cnt
  return $ (unValue . head) cnt'

saveSuspiciousPost :: Int -> Post -> IO ()
saveSuspiciousPost publicId' post = withDb $ do
  p <- getBy $ PostId (postId $ post)
  case p of
    Nothing -> do
      postId' <- insert $ DomPost (publicId') (postId $ post) (created post) (BSS.pack "") (authorId post) (authorName  post) (toSqlKey 0) (text post) True False
      _ <- mapM (\mediaRef -> insert $ DomMediaRef publicId' (postId') (artist mediaRef) (title $ mediaRef)) $ media post
      return ()
    Just _ -> return ()
  return ()

listPublics :: IO [DomPublic]
listPublics = withDb $ do
  ps <- select $
    from $ \domPublic -> do
    return domPublic
  return $ map entityVal ps
