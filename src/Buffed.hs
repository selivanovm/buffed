module Buffed (runFetching)
where

import System.Environment

import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import GHC.IO.Handle

import Control.Monad.Reader
import Control.Exception

import BuffedData
import VkPublicFetch

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

logFileHandler :: IO (GenericHandler Handle)
logFileHandler = do
  lh <- fileHandler "buffed.log" DEBUG
  return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

runFetching :: [String] -> IO ()
runFetching args = do
  catchAny ((\args ->
             case args of
              [ url, dsMode ] -> do
                bracket (logFileHandler) (\x -> ((putStrLn "Closing Logger") >> close x)) (\logHandler -> do
                  updateGlobalLogger "Fetcher" $ (setHandlers [logHandler]) . (setLevel DEBUG)
                  updateGlobalLogger "Parser" $ (setHandlers [logHandler]) . (setLevel DEBUG)
                  updateGlobalLogger "DbFunctions" $ (setHandlers [logHandler]) . (setLevel DEBUG)
                  runReaderT fetchWall Config { url = url, dsMode = read dsMode })

              _ -> putStrLn "Usage: buffed <url> <dumpResponse = true | false>") args) $ \e -> do
                     putStrLn $ "Got an exception: " ++ show e

main :: IO ()
main = getArgs >>= runFetching
