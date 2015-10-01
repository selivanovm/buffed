{-# LANGUAGE OverloadedStrings #-}

module VkPublicParse(parsePosts, parsePublicInfo) where

import           Text.HTML.TagSoup           hiding (parseTags, renderTags)
import           Text.HTML.TagSoup.Fast

import qualified Data.ByteString.Char8       as BSS

import           Data.Encoding (decodeStrictByteString)
import           Data.Encoding.UTF8

import           Data.Maybe

import           Data.Char
import           Data.String
import           Data.Text                   as Txt (Text)
import           Data.Text.Read              (decimal, signed)

import           Control.Monad.Extra         (liftMaybe)
import qualified Control.Monad.Parallel      as MP
import           Control.Monad.Trans         (lift)
import           Control.Monad.Trans.Maybe
import           Control.Parallel.Strategies

import           System.Log.Logger

import           VkPublicData

firstTag :: [[c]] -> c
firstTag = head . head

firstTagM :: [[a]] -> Maybe a
firstTagM as = listToMaybe as >>= listToMaybe

firstTextTagM :: [[Tag BSS.ByteString]] -> Maybe BSS.ByteString
firstTextTagM tags = do ts <- listToMaybe tags
                        ts <- listToMaybe (filter isTagText ts)
                        return (fromTagText ts)
listMaybe :: [a] -> Maybe [a]
listMaybe [] = Nothing
listMaybe l = Just l

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True a = Just a
boolToMaybe _ _ = Nothing


s :: String -> String
s = id

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

publicIdFromPostId :: Tag BSS.ByteString -> String
publicIdFromPostId tag = takeWhile (/='_') $ drop 4 $ BSS.unpack $ fromAttrib "id" tag

textEndingTags = [ s"<div class=page_post_sized_thumbs>"
                 , s"<div class=page_post_queue_wide>"
                 , s"<div class=page_post_sized_full_thumb>"
                 , s"<div class=page_post_sized_full_thumb_first>"
                 ]

parsePostData :: [Tag BSS.ByteString] -> MaybeT IO Post
parsePostData postTags = do

  cr' <- liftMaybe (headMaybe $ sections (~== s"<span class=rel_date>") postTags) :: MaybeT IO [Tag BSS.ByteString]
  created' <- liftMaybe (headMaybe (filter isTagText cr')) :: MaybeT IO (Tag BSS.ByteString)
  let extractCreated = innerText [ created' ]
      postDate = decodeStrictByteString UTF8 extractCreated
  createdDate <- lift $ parseDate postDate
  liftMaybe $ postMaybe createdDate

  where
    createUserRef tag = UserRef { accountLink = fromAttrib "href" $ head tag
                                , name = fromTagText $ head $ filter isTagText tag }
    signerTag = headMaybe $ sections (~== s"<a class=wall_signed_by>") postTags
    textTags = sections (~== s"<div class=wall_post_text>") postTags
    renderText t = renderTags $ takeWhile (\x -> any (x ~/=) textEndingTags) $ tail t
    tags = takeWhile (~/= s"<div class=wall_signed>") postTags
    postMaybe createdDate = do
      authorIdMaybe <- headMaybe <$> (headMaybe $ sections (~== s"<a class=author>") postTags)
      authorId' <- fromAttrib "data-from-id" <$> authorIdMaybe

      postId' <- fromAttrib "id" <$> headMaybe postTags
      let img' = mapMaybe (fmap (fromAttrib "src") . headMaybe) (sections (~== s"<img class=page_post_thumb_sized_photo>") postTags)
          signer' = fmap createUserRef signerTag
      authorName' <- innerText <$> maybeToList <$> headMaybe <$> filter isTagText <$> (headMaybe $ sections (~== s"<a class=author>") postTags)
      text' <- fmap renderText (headMaybe textTags) :: Maybe BSS.ByteString

      mediaElementSections <- listMaybe (sections (~== s"<div class=play_btn_wrap>") tags) :: Maybe [[Tag BSS.ByteString]]
      mediaElements <- listMaybe (mapMaybe (headMaybe . dropWhile (~/= s"<input>")) mediaElementSections) :: Maybe [Tag BSS.ByteString]
      titleElements <- (listMaybe $ mapMaybe (headMaybe . sections (~== s"<div class='title_wrap fl_l'>")) mediaElementSections) :: Maybe [[Tag BSS.ByteString]]
      elementInfos <- (listMaybe $ map (\x -> (fromTagText (x !! 3), fromTagText (x !! 8))) titleElements) :: Maybe [(BSS.ByteString, BSS.ByteString)]
      mediaRefElements <- (boolToMaybe (length mediaElements == length titleElements) (zip mediaElements elementInfos)) :: Maybe [(Tag BSS.ByteString, (BSS.ByteString, BSS.ByteString))]
      mediaRefs <- listMaybe $
        map (\x -> MediaRef { mediaLink = BSS.pack $ takeWhile (/= '?') $ BSS.unpack $ fromAttrib "value" (fst x), artist = (fst.snd) x, title = (snd.snd) x}) $
          mediaRefElements

      return Post { postId = BSS.pack . tail . dropWhile (/='_') . BSS.unpack $ postId'
                  , created = createdDate
                  , img = img'
                  , authorId = authorId'
                  , authorName = authorName'
                  , signer = signer'
                  , text = text'
                  , media = mediaRefs }

parsePublicInfo :: BSS.ByteString -> Maybe PublicInfo
parsePublicInfo html = do
  let tags = parseTags html
  pageNameText <- firstTextTagM $ sections (~== s"<div class='top_header page_name'>") tags
  wallHeaderTag <- firstTagM $ sections (~== s"<div class='post all own'>") tags
  publicId' <- publicIdMaybe $ parsePublicId wallHeaderTag
  return PublicInfo { publicName =  pageNameText, publicId = publicId' }
    where parsePublicId tag = (signed decimal $ fromString $ (publicIdString tag)) :: Either String (Int, Text)
          publicIdString tag = publicIdFromPostId tag
          publicIdMaybe e = case e of
            Right r -> Just $ fst r
            Left _ -> Nothing

extractPosts :: [Tag BSS.ByteString] -> [[Tag BSS.ByteString]]
extractPosts tags = parMap rseq (\x -> takeWhile (~/= s"<div class='replies_wrap clear'>") x) $ sections (~== s"<div class='post all own'>") tags


parsePosts :: BSS.ByteString -> IO FetchResult
parsePosts html = let tags = parseTags html
                      postTags = extractPosts tags
                      ownerId' = publicIdFromPostId $ firstTag postTags
                      offsetAttr = fromAttrib "onclick" $ firstTag $ sections (~== s"<a id=wall_more_link>") $ head postTags
                      offset' = BSS.pack $ takeWhile isDigit $ dropWhile (not . isDigit) $ BSS.unpack offsetAttr
                  in do
                    infoM "Parser" "Start parsing"
                    allPosts <- catMaybes <$> MP.mapM (runMaybeT . parsePostData) postTags
                    let result = if ((length allPosts) > 0)
                                 then ParsingResult { posts = allPosts, ownerId = BSS.pack ownerId', parsedAtOffset = offset' }
                                 else FetchingSuccess
                    infoM "Parser" "End parsing"
                    return result
