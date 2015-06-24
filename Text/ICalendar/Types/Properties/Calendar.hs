{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Calendar Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.7>
module Text.ICalendar.Types.Properties.Calendar
    ( CalScale(..)
    , Method(..)
    , ProdId(..)
    , Version(..)
    ) where

import           Data.CaseInsensitive            (CI)
import           Data.Default                    (Default (..))
import           Data.Text.Lazy                  (Text)
import           Data.Typeable                   (Typeable)
import qualified Data.Version                    as V
import           GHC.Generics                    (Generic)

import           Text.ICalendar.Types.Parameters

-- | Calendar Scale, 3.7.1.
data CalScale = CalScale
    { calScaleValue :: CI Text
    , calScaleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default CalScale where
    def = CalScale "GREGORIAN" def

-- | Method, 3.7.2.
data Method = Method
    { methodValue :: CI Text -- TODO: iTIP, RFC5546
    , methodOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Product Identifier, 3.7.3.
data ProdId = ProdId
    { prodIdValue :: Text
    , prodIdOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Version, 3.7.4.
data Version
    = MaxVersion
    { versionMax   :: V.Version
    , versionOther :: OtherParams
    }
    | MinMaxVersion
    { versionMax   :: V.Version
    , versionMin   :: V.Version
    , versionOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

