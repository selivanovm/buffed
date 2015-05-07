module BuffedData where

import qualified Data.ByteString.Char8 as BSS
import Data.Time

type FetchOffset = Int
type OwnerId = Int

type PostId = BSS.ByteString
type PostDate = UTCTime
data PublicState = Dirty OwnerId
                 | Clean OwnerId PostId PostDate

data DataSourceMode = DSNormal | DSToFile | DSFromFile
data Config = Config { url :: String, dsMode :: DataSourceMode }

instance Show DataSourceMode where
  show m = case m of
    DSToFile -> "toFile"
    DSFromFile -> "fromFile"
    DSNormal -> "normal"

instance Read DataSourceMode where
  readsPrec _ s = (case s of
    "toFile" -> DSToFile
    "fromFile" -> DSFromFile
    _ -> DSNormal, "") : []
