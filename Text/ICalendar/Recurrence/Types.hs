{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Text.ICalendar.Recurrence.Types
  (ICalError, TimeToOffset, TZIDToOffset, RItem(..), icalError) where

import           Data.Text.Lazy (Text)
import           Data.Time      (LocalTime, TimeZone, UTCTime)
import           Data.Typeable  (Typeable)
import           GHC.Generics   (Generic)


type ICalError = [String]
type TimeToOffset = LocalTime -> Either ICalError TimeZone
type TZIDToOffset = Text -> TimeToOffset

icalError :: String -> Either ICalError a
icalError s = Left [s]

-- RItem is a container for iterating over VRecurrence
-- rIRecurrence is the recurrence datatstructure
-- rIStartDateUtc and rIEndDateUTC are the times in UTC for ordering
-- Because of this they are specified first.
data RItem r =
  RItem
  { rIStartDateUtc :: Maybe UTCTime
  , rIEndDateUtc   :: Maybe UTCTime
  , rItem          :: r
  } deriving (Show, Eq, Ord, Typeable, Generic)
