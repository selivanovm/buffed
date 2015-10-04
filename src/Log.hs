module Log(ScopedLogger, initLogger, scopedLogger, doLogI, doLogD, doLogE) where

import GHC.IO.Handle

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter

type LogFn = String -> IO ()

data ScopedLogger = ScopedLogger { doLogD :: LogFn, doLogI :: LogFn, doLogE :: LogFn }

scopedLogger :: String -> ScopedLogger
scopedLogger scope = ScopedLogger { doLogD = debugM scope, doLogI = infoM scope, doLogE = errorM scope }

initLogger :: IO ()
initLogger = logFileHandler >>= prepareLogger

logFileHandler :: IO (GenericHandler Handle)
logFileHandler = do
  lh <- fileHandler "buffed.log" DEBUG
  return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")

prepareLogger :: (GenericHandler Handle) -> IO ()
prepareLogger logHandler = do
  updateGlobalLogger "Fetcher"     $ (setHandlers [logHandler]) . (setLevel DEBUG)
  updateGlobalLogger "Parser"      $ (setHandlers [logHandler]) . (setLevel DEBUG)
  updateGlobalLogger "Db"          $ (setHandlers [logHandler]) . (setLevel DEBUG)
  updateGlobalLogger "Download"    $ (setHandlers [logHandler]) . (setLevel DEBUG)
