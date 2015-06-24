{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Change Management Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.7>
module Text.ICalendar.Types.Properties.ChangeManagement
    ( Created(..)
    , DTStamp(..)
    , LastModified(..)
    , Sequence(..)
    ) where

import           Data.Default                    (Default (..))
import           Data.Time                       (UTCTime)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)

import           Text.ICalendar.Types.Parameters


-- | Date-Time Created. 3.8.7.1.
data Created = Created
    { createdValue :: UTCTime
    , createdOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Date-Time Stamp. 3.8.7.2.
data DTStamp = DTStamp
    { dtStampValue :: UTCTime
    , dtStampOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Last Modified. 3.8.7.3.
data LastModified = LastModified
    { lastModifiedValue :: UTCTime
    , lastModifiedOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Sequence number. 3.8.7.4.
data Sequence = Sequence
    { sequenceValue :: Integer
    , sequenceOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Sequence where
    def = Sequence 0 def
