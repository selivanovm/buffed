module Download (downloadPost) where

import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString       as B
import           Data.List.Split       (splitOn)
import           System.IO.Temp        (withSystemTempDirectory)
import           System.Directory      (createDirectoryIfMissing, renameFile)
import           Network.HTTP
import           Network.URI           (parseURI)

import           VkPublicFetch         (requestSinglePost)
import           Log                   (ScopedLogger, scopedLogger, doLogI, doLogD, doLogE)
import           DbFunctions           (setPostDownloaded)

type MediaFileLink = String
type MediaFilePath = String

downloadsDirectory :: FilePath
downloadsDirectory = "./downloads"

logger :: ScopedLogger
logger = scopedLogger "Download"

downloadPost :: Int -> BSS.ByteString -> IO ()
downloadPost publicId postId = do
  logger `doLogI` ("downloading post : publicId = " ++ show publicId ++ ", postId = " ++ BSS.unpack postId)
  fetchResult <- requestSinglePost publicId (read . BSS.unpack $ postId)
  logger `doLogD` "got a response"
  case fetchResult of
    Left errorMsg -> do
      logger `doLogE` ("Parsing ..." ++ errorMsg)
    Right links -> do
      logger `doLogD` "Links to download:"
      mapM_ (\link -> logger `doLogD` (BSS.unpack link)) links
      withSystemTempDirectory "buffed" (saveMediaFiles links)
      setPostDownloaded publicId postId

saveMediaFiles ::  [BSS.ByteString] -> FilePath -> IO ()
saveMediaFiles mediaLinks tmpDir = do
  mapM_ (\link -> downloadFile tmpDir (BSS.unpack link)) mediaLinks
  createDirectoryIfMissing False downloadsDirectory
  mapM_ (\link -> do
    let fileName = fileNameFromPath (BSS.unpack link)
    renameFile (tmpDir ++ "/" ++ fileName) (downloadsDirectory ++ "/" ++ fileName)
    return ()) mediaLinks
  return ()


get :: String -> IO B.ByteString
get url = let uri = case parseURI url of
                         Nothing -> error $ "Invalid URI: " ++ url
                         Just u -> u in
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

downloadFile :: String -> MediaFileLink -> IO (Either String MediaFilePath)
downloadFile tempDirectory mediaFileLink = do
  file <- get mediaFileLink
  let mediaFilePath = tempDirectory ++ "/" ++ (fileNameFromPath mediaFileLink)
  B.writeFile mediaFilePath file
  return $ Right mediaFilePath

fileNameFromPath :: String -> String
fileNameFromPath link = last $ splitOn "/" link
