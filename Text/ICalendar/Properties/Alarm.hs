{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Alarm Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.6>
module Text.ICalendar.Properties.Alarm
    (
    -- | Action is encoded directly in 'VAlarm'.
      Repeat(..)
    , Trigger(..)
    ) where

import           Data.Default                       (Default (..))
import           Data.Time                          (UTCTime)
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)

import           Text.ICalendar.Parameters
import           Text.ICalendar.Values


-- | Repeat count, 3.8.6.2.
data Repeat = Repeat
    { repeatValue :: Integer
    , repeatOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Repeat where
    def = Repeat 0 def

-- | Trigger, 3.8.6.3.
data Trigger
    = TriggerDuration
    { triggerDuration :: Duration
    , triggerRelated  :: Related -- ^ 'def' = 'Start'
    , triggerOther    :: OtherParameters
    }
    | TriggerDateTime
    { triggerDateTime :: UTCTime
    , triggerOther    :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

