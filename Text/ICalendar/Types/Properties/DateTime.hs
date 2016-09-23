{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Date and Time Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.2>
module Text.ICalendar.Types.Properties.DateTime
    ( VDateTime(..)
    , Completed(..)
    , DTEnd(..)
    , Due(..)
    , DTStart(..)
    , DurationProp(..)
    , FreeBusy(..)
    , Transp(..)
    , updateEndTime
    ) where

import           Data.Default                    (Default (..))
import           Data.Set                        (Set)
import           Data.Time
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)

import           Text.ICalendar.Types.Parameters
import           Text.ICalendar.Types.Values

data VDateTime
    = VDateTime { vDateTimeValue :: DateTime }
    | VDate { vDateValue :: Date}
     deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time Completed. 3.8.2.1.
data Completed = Completed
    { completedValue :: DateTime
    , completedOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time End. 3.8.2.2.
data DTEnd = DTEnd
  { dtEndValue :: VDateTime
  , dtEndOther :: OtherParams
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time Due, 3.8.2.3.
data Due = Due
  { dueValue :: VDateTime
  , dueOther :: OtherParams
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time Start, 3.8.2.4.
data DTStart = DTStart
  { dtStartValue :: VDateTime
  , dtStartOther :: OtherParams
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

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


updateEndTime :: DTStart -> DTStart -> DTEnd -> DTEnd
updateEndTime (DTStart ds1 _) (DTStart ds2 _) (DTEnd de o) = DTEnd (_updateVDate ds1 ds2 de) o

_updateVDate :: VDateTime -> VDateTime -> VDateTime -> VDateTime
_updateVDate (VDate ds1) (VDate ds2) (VDate de) = VDate (_updateDate ds1 ds2 de)
_updateVDate (VDateTime ds1) (VDateTime ds2) (VDateTime de) = VDateTime (_updateDateTime ds1 ds2 de)
_updateVDate ds1 ds2 dtend = error $ "Mismatched Date types:" ++ show ds1 ++ show ds2 ++ show dtend

_updateDate :: Date -> Date -> Date -> Date
_updateDate (Date ds1) (Date ds2) (Date de) = Date (addDays (diffDays ds2 ds1) de)

_updateDateTime :: DateTime -> DateTime -> DateTime -> DateTime
_updateDateTime (UTCDateTime ds1) (UTCDateTime ds2) (UTCDateTime de) = UTCDateTime (addUTCTime (diffUTCTime ds2 ds1) de)
_updateDateTime (FloatingDateTime ds1) (FloatingDateTime ds2) (FloatingDateTime de) = FloatingDateTime (_updateLocalTime ds1 ds2 de)
_updateDateTime (ZonedDateTime ds1 _) (ZonedDateTime ds2 _) (ZonedDateTime de z) = ZonedDateTime (_updateLocalTime ds1 ds2 de) z
_updateDateTime ds1 ds2 dtend = error $ "Mismatched Date types:" ++ show ds1 ++ show ds2 ++ show dtend

_updateLocalTime :: LocalTime -> LocalTime -> LocalTime -> LocalTime
_updateLocalTime ds1 ds2 de = utcToLocalTime utc (addUTCTime (diffUTCTime (f ds2) (f ds1)) (f de))
  where f = localTimeToUTC utc
