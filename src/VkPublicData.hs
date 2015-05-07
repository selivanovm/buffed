module VkPublicData where

import Data.Text()
import qualified Data.ByteString.Char8 as BSS

import System.Locale
import Data.Time
import Data.List.Split

import Debug.Trace


data UserRef = UserRef { accountLink :: BSS.ByteString
                       , name :: BSS.ByteString } deriving (Show)

data MediaRef = MediaRef { mediaLink :: BSS.ByteString
                         , artist :: BSS.ByteString
                         , title :: BSS.ByteString
                         } deriving (Show)

data Post = Post { postId :: BSS.ByteString
                 , created :: UTCTime
                 , img :: [BSS.ByteString]
                 , authorId :: BSS.ByteString
                 , authorName :: BSS.ByteString
                 , signer :: Maybe UserRef
                 , text :: BSS.ByteString
                 , media :: [MediaRef]
                 } deriving (Show)


data PublicInfo = PublicInfo { publicName :: BSS.ByteString
                             , publicId :: Int
                             } deriving Show

data FetchResult = FetchingFail | FetchingSuccess | ParsingResult { posts :: [Post]
                                                                  , ownerId :: BSS.ByteString
                                                                  , parsedAtOffset :: BSS.ByteString
                                                                  } deriving Show

parseDate :: String -> IO UTCTime
parseDate dateString = trace (dateString ++ "!!!") $ case dateString of
  ('с':'е':'г':'о':'д':'н':'я':' ':'в':' ':time) -> parseTodayTime time
  ('в':'ч':'е':'р':'а':' ':'в':' ':time) -> parseYesterdayTime time
  (string) -> case (splitOn " " string) of
    (day:month:_:time:[]) -> parseThisYearTime (addLeadingZero day) month time
    (day:month:year:[]) -> parsePastYearTime (addLeadingZero day) month year


addLeadingZero :: String -> String
addLeadingZero (x:[]) = '0':x:[]
addLeadingZero (x:y:[]) = x:y:[]
addLeadingZero h = trace (">>> wrong time" ++ h) h

parsePostTime :: String -> TimeOfDay
parsePostTime timeString = readTime defaultTimeLocale "%k:%M" timeString :: TimeOfDay

serverTimeZone :: TimeZone
serverTimeZone = hoursToTimeZone 3

serverTime :: UTCTime -> LocalTime
serverTime currentUTCTime = utcToLocalTime serverTimeZone currentUTCTime

parseTodayTime :: String -> IO UTCTime
parseTodayTime timeString = do
  currentUTCTime <- getCurrentTime
  let day = localDay $ serverTime currentUTCTime
  let today = LocalTime { localDay = day
                        , localTimeOfDay = parsePostTime timeString }

  return $ localTimeToUTC serverTimeZone today

parseYesterdayTime :: String -> IO UTCTime
parseYesterdayTime timeString = do
  currentUTCTime <- getCurrentTime
  let day = localDay $ serverTime currentUTCTime
  let yesterday = LocalTime { localDay = (addDays (-1) day)
                            , localTimeOfDay = parsePostTime timeString }
  return $ localTimeToUTC serverTimeZone yesterday

parseThisYearTime :: String -> String -> String -> IO UTCTime
parseThisYearTime day month time = do
  currentUTCTime <- getCurrentTime
  let year = formatTime defaultTimeLocale "%Y" $ serverTime currentUTCTime
  let dateString = year ++ " " ++ (monthSignatureToNumber month) ++ " " ++ day ++ " " ++ time
  let serverLocalTime = readTime defaultTimeLocale "%Y %m %d %k:%M" dateString :: LocalTime
  return $ localTimeToUTC serverTimeZone serverLocalTime

parsePastYearTime :: String -> String -> String -> IO UTCTime
parsePastYearTime day month year = do
  return $ readTime defaultTimeLocale "%Y %m %d" $ year ++ " " ++ (monthSignatureToNumber month) ++ " " ++ day

monthSignatureToNumber :: String -> String
monthSignatureToNumber monthSignature = trace monthSignature $ case monthSignature of
  "янв" -> "01"
  "фев" -> "02"
  "мар" -> "03"
  "апр" -> "04"
  "мая" -> "05"
  "июн" -> "06"
  "июл" -> "07"
  "авг" -> "08"
  "сен" -> "09"
  "окт" -> "10"
  "ноя" -> "11"
  "дек" -> "12"
