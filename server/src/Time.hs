{-# LANGUAGE DeriveGeneric #-}

module Time where

import Data.Aeson.Compat (ToJSON)
import Data.Fixed
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime(..), utcToLocalTime, getCurrentTimeZone, TimeOfDay(..))
import GHC.Generics

data DateTime = DateTime 
  { date :: Date
  , time :: Time
  } deriving (Eq, Ord, Show, Generic)

data Date = Date 
  { year :: Integer
  , month :: Int
  , day :: Int 
  } deriving (Eq, Ord, Show, Generic)

data Time = Time
  { h :: Int
  , m :: Int
  , s :: Int
  , ml :: Int
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON DateTime
instance ToJSON Date
instance ToJSON Time

toDateTime :: LocalTime -> DateTime
toDateTime (LocalTime lday tOfDay) =
  DateTime
    (Date year month day)
    (Time h m sec mill)
  where (year, month, day) = toGregorian lday
        (TimeOfDay h m s) = tOfDay
        mill = picoToMilli s `mod` 1000
        sec = picoToMilli s `div` 1000

getDateTime :: IO DateTime
getDateTime = do
  ltime <- getLocalTime
  return $ toDateTime ltime

getLocalTime :: IO LocalTime
getLocalTime = do
  utc <- getCurrentTime
  tzone <- getCurrentTimeZone
  return $ utcToLocalTime tzone utc

picoToMilli :: Pico -> Int
picoToMilli p = floor $ toRational $ p * 1000