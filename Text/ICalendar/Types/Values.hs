{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | iCalendar property value types.
--
-- <https://tools.ietf.org/html/rfc5545#section-3.3>
module Text.ICalendar.Types.Values
    (
    -- | Omitted:
    -- @
    --     Binary = ByteString (only present base64-decoded)
    --     Boolean = 'Bool'
    --     CalAddress the plain value = 'URI'
    -- @
      CalAddress -- (..)
    , Date(..)
    , DateTime(..)
    , Duration(..)
    , Sign(..)
    -- | Omitted:
    --
    -- @
    --     Float = 'Float'
    --     Integer = 'Int'
    -- @
    , Period(..)
    , UTCPeriod(..)
    -- * Recurrence rules
    , Recur(..)
    , Freq(..)
    , Weekday(..)
    -- | Omitted:
    --
    -- @
    --     Text = 'Text'
    --     Time = ???
    --     URI = 'URI'
    -- @
    , UTCOffset(..)
    -- * Values that are really Text but that can be refined further:
    , ClassValue(..)
    ) where

import           Data.CaseInsensitive            (CI)
import           Data.Default                    (Default (..))
import           Data.Text.Lazy                  (Text)
import           Data.Time                       (Day, LocalTime, UTCTime)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Network.URI                     (URI)

import           Text.ICalendar.Types.Parameters

{-
-- | Calendar User Address, 3.3.3.
data CalAddress
    = CalAddress
    { calAddressValue :: URI
      -- TODO: move all parameters that can be on a CalAddress into this from
      -- the property.
    } deriving (Show, Eq, Ord, Typeable, Generic)
-}
type CalAddress = URI

-- | Date, 3.3.4.
data Date
    = Date
    { dateValue :: Day
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time, 3.3.5.
data DateTime
    = FloatingDateTime
    { dateTimeFloating :: LocalTime
    }
    | UTCDateTime
    { dateTimeUTC :: UTCTime
    }
    | ZonedDateTime
    { dateTimeFloating :: LocalTime
    , dateTimeZone     :: Text
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Duration, 3.3.6.
data Duration -- TODO(?): Convert to DiffTime?
    = DurationDate
    { durSign   :: Sign -- ^ 'def' = 'Positive'
    , durDay    :: Int
    , durHour   :: Int
    , durMinute :: Int
    , durSecond :: Int
    }
    | DurationTime
    { durSign   :: Sign
    , durHour   :: Int
    , durMinute :: Int
    , durSecond :: Int
    }
    | DurationWeek
    { durSign :: Sign
    , durWeek :: Int
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Sign, in duration, 3.3.10.
data Sign = Positive | Negative
            deriving (Show, Eq, Ord, Typeable, Generic)

-- | 'Positive'.
instance Default Sign where
    def = Positive

-- | Period of time, 3.3.9.
data Period
    = PeriodDates    DateTime DateTime
    | PeriodDuration DateTime Duration
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Period of time which must be UTC, as in FreeBusy, 3.3.9.
data UTCPeriod
    = UTCPeriodDates    UTCTime UTCTime
    | UTCPeriodDuration UTCTime Duration
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Recur, 3.3.10.
data Recur = Recur
    { recurFreq       :: Freq
    , recurUntilCount :: Maybe (Either (Either Date DateTime) Int)
    , recurInterval   :: Int
    , recurBySecond   :: [Int]
    , recurByMinute   :: [Int]
    , recurByHour     :: [Int]
    , recurByDay      :: [Either (Int, Weekday) Weekday]
    , recurByMonthDay :: [Int]
    , recurByYearDay  :: [Int]
    , recurByWeekNo   :: [Int]
    , recurByMonth    :: [Int]
    , recurBySetPos   :: [Int]
    , recurWkSt       :: Weekday
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Frequency, in recurrences, 3.3.10.
data Freq
    = Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Weekday, in recurrences, 3.3.10.
data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday
                      | Friday | Saturday
               deriving (Show, Eq, Ord, Bounded, Enum, Typeable, Generic)

-- | UTC Offset, 3.3.14
--
-- TODO: clean, don't use as TZOffsetTo/TZOffsetFrom
data UTCOffset = UTCOffset
    { utcOffsetValue :: Int -- ^ Number of seconds away from UTC
    , utcOffsetOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Classification value. 3.8.1.3.
--
-- Unrecognized ClassValueX MUST be treated as Private.
data ClassValue
    = Public
    | Private
    | Confidential
    | ClassValueX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | 'Public'.
instance Default ClassValue where
    def = Public
