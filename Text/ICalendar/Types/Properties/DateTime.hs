{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Date and Time Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.2>
module Text.ICalendar.Types.Properties.DateTime
    ( Completed(..)
    , DTEnd(..)
    , Due(..)
    , DTStart(..)
    , DurationProp(..)
    , FreeBusy(..)
    , Transp(..)
    ) where

import           Data.Default                    (Default (..))
import           Data.Set                        (Set)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)

import           Text.ICalendar.Types.Parameters
import           Text.ICalendar.Types.Values

-- | Date-Time Completed. 3.8.2.1.
data Completed = Completed
    { completedValue :: DateTime
    , completedOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time End. 3.8.2.2.
data DTEnd
    = DTEndDateTime
    { dtEndDateTimeValue :: DateTime
    , dtEndOther         :: OtherParams
    }
    | DTEndDate
    { dtEndDateValue :: Date
    , dtEndOther     :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time Due, 3.8.2.3.
data Due
    = DueDateTime
    { dueDateTimeValue :: DateTime
    , dueOther         :: OtherParams
    }
    | DueDate
    { dueDateValue :: Date
    , dueOther     :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time Start, 3.8.2.4.
data DTStart
    = DTStartDateTime
    { dtStartDateTimeValue :: DateTime
    , dtStartOther         :: OtherParams
    }
    | DTStartDate
    { dtStartDateValue :: Date
    , dtStartOther     :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Duration, 3.8.2.5.
data DurationProp = DurationProp
    { durationValue :: Duration
    , durationOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Free/Busy Time, 3.8.2.6
data FreeBusy = FreeBusy
    { freeBusyType    :: FBType
    , freeBusyPeriods :: Set UTCPeriod
    , freeBusyOther   :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Time Transparency. 3.8.2.7.
data Transp
    = Opaque      { timeTransparencyOther :: OtherParams }
    | Transparent { timeTransparencyOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | 'Opaque'
instance Default Transp where
    def = Opaque def
