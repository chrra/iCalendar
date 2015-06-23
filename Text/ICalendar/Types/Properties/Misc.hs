{-# LANGUAGE DeriveGeneric #-}
-- | Miscellaneous Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.8>
module Text.ICalendar.Types.Properties.Misc
    ( OtherProperty(..)
    , RequestStatus(..)
    ) where

import           Data.ByteString.Lazy.Char8      (ByteString)
import           Data.CaseInsensitive            (CI)
import           Data.Text.Lazy                  (Text)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)

import           Text.ICalendar.Types.Parameters


-- | Any other property, 3.8.8.1, 3.8.8.2
data OtherProperty = OtherProperty
    { otherName   :: CI Text
    , otherValue  :: ByteString
    , otherParams :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Request Status. 3.8.8.3.
data RequestStatus = RequestStatus
    { requestStatusCode     :: [Int]
    , requestStatusDesc     :: Text
    , requestStatusLanguage :: Maybe Language
    , requestStatusExt      :: Maybe Text
    , requestStatusOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)
