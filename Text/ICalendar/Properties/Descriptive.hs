{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
-- | Descriptive Component Properties.
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.1>
module Text.ICalendar.Properties.Descriptive
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

import           Codec.MIME.Type                    (MIMEType)
import           Data.ByteString.Lazy.Char8         (ByteString)
import           Data.Default                       (Default (..))
import           Data.Set                           (Set)
import           Data.Text.Lazy                     (Text)
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Network.URI                        (URI)

import           Text.ICalendar.Parameters
import           Text.ICalendar.Values


-- | Attachment, 3.8.1.1.
data Attach
    = UriAttach
    { attachFmtType :: Maybe MIMEType
    , attachUri     :: URI
    , attachOther   :: OtherParameters
    }
    | BinaryAttach
    { attachFmtType :: Maybe MIMEType
    , attachContent :: ByteString
    , attachOther   :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- instance ICalendarProperty Attach where


-- | Categories, 3.8.1.2.
data Categories = Categories
    { categoriesValues   :: Set Text
    , categoriesLanguage :: Maybe Language
    , categoriesOther    :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Classification, 3.8.1.3.
data Class = Class
    { classValue :: ClassValue
    , classOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Class where
    def = Class def def

-- | Comment, 3.8.1.4.
data Comment = Comment
    { commentValue    :: Text
    , commentAltRep   :: Maybe URI
    , commentLanguage :: Maybe Language
    , commentOther    :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Description, 3.8.1.5.
data Description = Description
    { descriptionValue    :: Text
    , descriptionAltRep   :: Maybe URI
    , descriptionLanguage :: Maybe Language
    , descriptionOther    :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Geographic Position, 3.8.1.6.
data Geo = Geo
    { geoLat   :: Float
    , geoLong  :: Float
    , geoOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Location, 3.8.1.7.
data Location = Location
    { locationValue    :: Text
    , locationAltRep   :: Maybe URI
    , locationLanguage :: Maybe Language
    , locationOther    :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Percent complete, 3.8.1.8.
data PercentComplete = PercentComplete
    { percentCompleteValue :: Int
    , percentCompleteOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Priority, 3.8.1.9.
data Priority = Priority
    { priorityValue :: Int
    , priorityOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | 0
instance Default Priority where
    def = Priority 0 def

-- | Resources, 3.8.1.10.
data Resources = Resources
    { resourcesValue    :: Set Text
    , resourcesAltRep   :: Maybe URI
    , resourcesLanguage :: Maybe Language
    , resourcesOther    :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Status, but only for Events, 3.8.1.11.
data EventStatus
    = TentativeEvent { eventStatusOther :: OtherParameters }
    | ConfirmedEvent { eventStatusOther :: OtherParameters }
    | CancelledEvent { eventStatusOther :: OtherParameters }
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Status, but only for TODOs, 3.8.1.11.
data TodoStatus
    = TodoNeedsAction { todoStatusOther :: OtherParameters }
    | CompletedTodo   { todoStatusOther :: OtherParameters }
    | InProcessTodo   { todoStatusOther :: OtherParameters }
    | CancelledTodo   { todoStatusOther :: OtherParameters }
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Status, but only for Journals, 3.8.1.11.
data JournalStatus
    = DraftJournal     { journalStatusOther :: OtherParameters }
    | FinalJournal     { journalStatusOther :: OtherParameters }
    | CancelledJournal { journalStatusOther :: OtherParameters }
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Summary, 3.8.1.12.
data Summary = Summary
    { summaryValue    :: Text
    , summaryAltRep   :: Maybe URI
    , summaryLanguage :: Maybe Language
    , summaryOther    :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)
