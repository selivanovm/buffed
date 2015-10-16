{-# LANGUAGE OverloadedStrings #-}

module VkPublicParse(parsePosts, parsePublicInfo, mediaLinksFromHTML) where

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

headOr :: [a] -> a -> a
headOr xs defaultValue = case xs of
  [] -> defaultValue
  _ -> head xs

firstTag :: [[c]] -> c
firstTag = head . head

firstTagM :: [[a]] -> Maybe a
firstTagM as = listToMaybe as >>= listToMaybe

firstTextTagM :: [[Tag BSS.ByteString]] -> Maybe BSS.ByteString
firstTextTagM tags = do ts <- listToMaybe tags
                        ts' <- listToMaybe (filter isTagText ts)
                        return (fromTagText ts')

listMaybe :: [a] -> Maybe [a]
listMaybe [] = Nothing
listMaybe l = Just l

s :: String -> String
s = id

publicIdFromPostId :: Tag BSS.ByteString -> String
publicIdFromPostId tag = takeWhile (/='_') $ drop 4 $ BSS.unpack $ fromAttrib "id" tag

textEndingTags :: [String]
textEndingTags = [ s"<div class=page_post_sized_thumbs>"
                 , s"<div class=page_post_queue_wide>"
                 , s"<div class=page_post_sized_full_thumb>"
                 , s"<div class=page_post_sized_full_thumb_first>"
                 ]

mediaElementSectionsFromTags :: [Tag BSS.ByteString] -> [[Tag BSS.ByteString]]
mediaElementSectionsFromTags = sections (~== s"<div class=play_btn_wrap>")

mediaElementsFromTags :: [[Tag BSS.ByteString]] -> Maybe [Tag BSS.ByteString]
mediaElementsFromTags mediaElementSections = listMaybe (mapMaybe (listToMaybe . dropWhile (~/= s"<input>")) $ mediaElementSections)

mediaLinkFromTag :: Tag BSS.ByteString -> BSS.ByteString
mediaLinkFromTag tag = BSS.pack $ takeWhile (/= '?') $ BSS.unpack $ fromAttrib "value" tag

mediaLinksFromHTML :: BSS.ByteString -> [BSS.ByteString]
mediaLinksFromHTML html = case (mediaElementsFromTags . mediaElementSectionsFromTags . parseTags $ html) of
                            Nothing -> []
                            Just links -> map mediaLinkFromTag links

parsePostData :: [Tag BSS.ByteString] -> MaybeT IO Post
parsePostData postTags = do

  cr' <- liftMaybe (listToMaybe $ sections (~== s"<span class=rel_date>") postTags) :: MaybeT IO [Tag BSS.ByteString]
  created' <- liftMaybe (listToMaybe (filter isTagText cr')) :: MaybeT IO (Tag BSS.ByteString)
  let extractCreated = innerText [ created' ]
      postDate = decodeStrictByteString UTF8 extractCreated
  createdDate <- lift $ parseDate postDate
  liftMaybe $ postMaybe createdDate

  where
    createUserRef tag = UserRef { accountLink = fromAttrib "href" $ head tag
                                , name = fromTagText $ head $ filter isTagText tag }
    signerTag = listToMaybe $ sections (~== s"<a class=wall_signed_by>") postTags
    textTags = sections (~== s"<div class=wall_post_text>") postTags
    renderText t = renderTags $ takeWhile (\x -> not $ any (x ~==) textEndingTags) $ tail t
    postMaybe createdDate = do
      authorIdMaybe <- listToMaybe <$> (listToMaybe $ sections (~== s"<a class=author>") postTags)
      authorId' <- fromAttrib "data-from-id" <$> authorIdMaybe

      postId' <- fromAttrib "id" <$> listToMaybe postTags
      let postQueueWide = takeWhile (~/= s"<div class=page_post_queue_narrow") (headOr (sections (~== s"<div class=page_post_queue_wide>") postTags) [])
          img' = mapMaybe (fmap (fromAttrib "src") . listToMaybe) (sections (~== s"<img class=page_post_thumb_sized_photo>") postQueueWide)
          signer' = fmap createUserRef signerTag
      authorName' <- innerText <$> maybeToList <$> listToMaybe <$> filter isTagText <$> (listToMaybe $ sections (~== s"<a class=author>") postTags)
      text' <- fmap renderText (listToMaybe textTags) :: Maybe BSS.ByteString

      return Post { postId = BSS.pack . tail . dropWhile (/='_') . BSS.unpack $ postId'
                  , created = createdDate
                  , img = concatImages img'
                  , authorId = authorId'
                  , authorName = authorName'
                  , signer = signer'
                  , text = text' }

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
                    debugM "Parser" $ "Input : " ++ (BSS.unpack html)
                    infoM "Parser" "Start parsing"
                    allPosts <- catMaybes <$> MP.mapM (runMaybeT . parsePostData) postTags
                    let result = if ((length allPosts) > 0)
                                 then ParsingResult { posts = allPosts, ownerId = BSS.pack ownerId', parsedAtOffset = offset' }
                                 else FetchingSuccess
                    infoM "Parser" "End parsing"
                    return result
