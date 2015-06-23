{-# LANGUAGE DeriveGeneric #-}
-- | Recurrence Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.5>
module Text.ICalendar.Types.Properties.Recurrence
    ( ExDate(..)
    , RDate(..)
    , RRule(..)
    ) where

import           Data.Set                        (Set)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)

import           Text.ICalendar.Types.Parameters
import           Text.ICalendar.Types.Values

-- | Exception Date-Times, 3.8.5.1.
data ExDate
    = ExDates
    { exDates     :: Set Date
    , exDateOther :: OtherParams
    }
    | ExDateTimes
    { exDateTimes :: Set DateTime
    , exDateOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Recurrence Date-Times, 3.8.5.2.
data RDate
    = RDateDates
    { rDateDates :: Set Date
    , rDateOther :: OtherParams
    }
    | RDateDateTimes
    { rDateDateTimes :: Set DateTime
    , rDateOther     :: OtherParams
    }
    | RDatePeriods
    { rDatePeriods :: Set Period
    , rDateOther   :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)


-- | Recurrence Rule, 3.8.5.3.
data RRule = RRule
    { rRuleValue :: Recur
    , rRuleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

