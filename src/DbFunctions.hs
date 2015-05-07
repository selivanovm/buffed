{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

module DbFunctions where

import qualified Data.ByteString.Char8 as BSS

import Data.Text(Text)
import Data.Time
import Data.Int(Int64)

import Control.Monad.IO.Class (liftIO)

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
   PostId     postId
   deriving Show
|]

withDb = runSqlite "buffed.db"

openDb :: IO ()
openDb = withDb $ do
       runMigration migrateTables

getPublicById :: Int -> IO (Maybe DomPublic)
getPublicById publicId' = withDb $ do
  domPublic <- getBy $ PublicId publicId'
  return $ fmap entityVal $ domPublic

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
  liftIO $ infoM "DbFunctions" $ "Removing all data for public with id = " ++ (show $ domPublicPublicId $ public)
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
            liftIO $ infoM "DbFunctions" $ "Clean public. Last post date " ++ (show postCreated')
            return $ Clean (domPublicPublicId public) postId' postCreated'
  | otherwise = do
    liftIO $ infoM "DbFunctions" $ "Dirty public"
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

savePost :: Int -> Post -> IO ()
savePost publicId' post = withDb $ do
  postId' <- insert $ DomPost (publicId') (postId $ post) (created post) (BSS.pack "") (authorId post) (authorName  post) (Key $ PersistInt64 0) (text post)
  _ <- mapM (\mediaRef -> insert $ DomMediaRef publicId' (postId') (artist mediaRef) (title $ mediaRef)) $ media post
  return ()

loadPosts :: Int -> Int -> IO [(DomPost, DomPublic)]
loadPosts recordOffset postsCount = withDb $ do
  ps <- select $
    from $ \(domPost `InnerJoin` domPublic) -> do
      on (domPost ^. DomPostPublicId ==. domPublic ^. DomPublicPublicId)
      orderBy [ desc (domPost ^. DomPostCreated) ]
      offset (fromIntegral recordOffset)
      limit (fromIntegral postsCount)
      return (domPost, domPublic)
  return $ map (\x -> (entityVal (fst x), entityVal (snd x))) ps

totalPostsCount :: IO Int
totalPostsCount = withDb $ do
  cntt <-
    select $ from $ \(_ :: SqlExpr (Entity DomPost)) -> do
      let cnt = countRows :: SqlExpr (Value Int)
      return cnt
  return $ (unValue . head) cntt

saveSuspiciousPost :: Int -> Post -> IO ()
saveSuspiciousPost publicId' post = withDb $ do
  p <- getBy $ PostId (postId $ post)
  case p of
    Nothing -> do
      postId' <- insert $ DomPost (publicId') (postId $ post) (created post) (BSS.pack "") (authorId post) (authorName  post) (Key $ PersistInt64 0) (text post)
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
