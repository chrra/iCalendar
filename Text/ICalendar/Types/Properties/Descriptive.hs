{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Descriptive Component Properties.
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.1>
module Text.ICalendar.Types.Properties.Descriptive
    ( Attach(..)
    , Categories(..)
    , Class(..)
    , Comment(..)
    , Description(..)
    , Geo(..)
    , Location(..)
    , PercentComplete(..)
    , Priority(..)
    , Resources(..)
    , EventStatus(..)
    , TodoStatus(..)
    , JournalStatus(..)
    , Summary(..)
    ) where

import           Codec.MIME.Type                 (MIMEType)
import           Data.ByteString.Lazy.Char8      (ByteString)
import           Data.Default                    (Default (..))
import           Data.Set                        (Set)
import           Data.Text.Lazy                  (Text)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Network.URI                     (URI)

import           Text.ICalendar.Types.Parameters
import           Text.ICalendar.Types.Values


-- | Attachment, 3.8.1.1.
data Attach
    = UriAttach
    { attachFmtType :: Maybe MIMEType
    , attachUri     :: URI
    , attachOther   :: OtherParams
    }
    | BinaryAttach
    { attachFmtType :: Maybe MIMEType
    , attachContent :: ByteString
    , attachOther   :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Categories, 3.8.1.2.
data Categories = Categories
    { categoriesValues   :: Set Text
    , categoriesLanguage :: Maybe Language
    , categoriesOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Classification, 3.8.1.3.
data Class = Class
    { classValue :: ClassValue
    , classOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Class where
    def = Class def def

-- | Comment, 3.8.1.4.
data Comment = Comment
    { commentValue    :: Text
    , commentAltRep   :: Maybe URI
    , commentLanguage :: Maybe Language
    , commentOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Description, 3.8.1.5.
data Description = Description
    { descriptionValue    :: Text
    , descriptionAltRep   :: Maybe URI
    , descriptionLanguage :: Maybe Language
    , descriptionOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Geographic Position, 3.8.1.6.
data Geo = Geo
    { geoLat   :: Float
    , geoLong  :: Float
    , geoOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Location, 3.8.1.7.
data Location = Location
    { locationValue    :: Text
    , locationAltRep   :: Maybe URI
    , locationLanguage :: Maybe Language
    , locationOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Percent complete, 3.8.1.8.
data PercentComplete = PercentComplete
    { percentCompleteValue :: Int
    , percentCompleteOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Priority, 3.8.1.9.
data Priority = Priority
    { priorityValue :: Int
    , priorityOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | 0
instance Default Priority where
    def = Priority 0 def

-- | Resources, 3.8.1.10.
data Resources = Resources
    { resourcesValue    :: Set Text
    , resourcesAltRep   :: Maybe URI
    , resourcesLanguage :: Maybe Language
    , resourcesOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Status, but only for Events, 3.8.1.11.
data EventStatus
    = TentativeEvent { eventStatusOther :: OtherParams }
    | ConfirmedEvent { eventStatusOther :: OtherParams }
    | CancelledEvent { eventStatusOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Status, but only for TODOs, 3.8.1.11.
data TodoStatus
    = TodoNeedsAction { todoStatusOther :: OtherParams }
    | CompletedTodo   { todoStatusOther :: OtherParams }
    | InProcessTodo   { todoStatusOther :: OtherParams }
    | CancelledTodo   { todoStatusOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Status, but only for Journals, 3.8.1.11.
data JournalStatus
    = DraftJournal     { journalStatusOther :: OtherParams }
    | FinalJournal     { journalStatusOther :: OtherParams }
    | CancelledJournal { journalStatusOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Summary, 3.8.1.12.
data Summary = Summary
    { summaryValue    :: Text
    , summaryAltRep   :: Maybe URI
    , summaryLanguage :: Maybe Language
    , summaryOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)
