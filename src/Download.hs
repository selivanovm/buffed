module Download (downloadPost) where

import qualified Data.ByteString.Char8 as BSS
import           DbFunctions           (getPostOffset)
import           VkPublicData
import           VkPublicFetch         (getNextPosts)
import           VkPublicParse         (parsePosts)

type SearchOffset = Int
type PostId = BSS.ByteString
type PublicId = Int

downloadPost :: Int -> BSS.ByteString -> IO ()
downloadPost publicId postId = do
  putStrLn $ "downloading post : publicId = " ++ show publicId ++ ", postId = " ++ BSS.unpack postId
  offset <- getPostOffset publicId postId
  return ()
{--
  case offset of
    Just offset' -> do
      let posts = searchPost publicId postId offset'
      return ()
    _ -> return ()
--}


   -- make binary search of post id where post date is an index
  -- 0 - update feed
  -- 1 - get offset of the post from db, postDate = created post
  -- 2 - get chunk of posts at that offset from vk
  -- 3 - check if this chunk contains the post, if yes then return it otherwise proceed
  -- 4 - (maxDate, minDate) = dateRange postsChunk
  -- 5 - if maxDate < postDate => binarySearch post range(minOffset -> currentOffset)
  --     else if minDate > postDate => binarySearch post range(currentOffset -> maxOffset)
  --     else if chunk contains the post => return it, otherwise return nothing

{--
searchPost :: Post -> SearchOffset -> IO SearchStatus
searchPost publicId postId offset = do
  putStrLn $ "post offset = " ++ show offset
  rawPosts <- getNextPosts publicId offset
  posts <- parsePosts rawPosts
  case posts of
    ParsingResult ps _ _ -> do
      mapM_ putStrLn . show . created ps
      return Fail
    _ -> do
      putStrLn "Parsing Error"
      return Fail
--}
