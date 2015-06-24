{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Time Zone Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.3>
module Text.ICalendar.Types.Properties.TimeZone
    ( TZID(..)
    , TZName(..)
    -- , TZOffsetFrom(..)
    -- , TZOffsetTo(..)
    , TZUrl(..)
    ) where

import           Data.Text.Lazy                  (Text)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Network.URI                     (URI)

import           Text.ICalendar.Types.Parameters


-- | Time Zone Identifier, 3.8.3.1.
data TZID = TZID
    { tzidValue  :: Text -- ^ Full name, including solidus if present.
    , tzidGlobal :: Bool -- ^ If the solidus, indicating globalness, is present.
    , tzidOther  :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Time Zone Name, 3.8.3.2.
data TZName = TZName
    { tzNameValue    :: Text
    , tzNameLanguage :: Maybe Language
    , tzNameOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

{-

data TZOffsetFrom = TZOffsetFrom
    { tzOffsetFromValue :: UTCOffset
    , tzOffsetFromOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

data TZOffsetTo = TZOffsetTo
    { tzOffsetToValue :: UTCOffset
    , tzOffsetToOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-}

-- | Time Zone URL, 3.8.3.5.
data TZUrl = TZUrl
    { tzUrlValue :: URI
    , tzUrlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)
