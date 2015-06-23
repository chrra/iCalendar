{-# LANGUAGE DeriveGeneric #-}
-- | Relationship Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.4>
module Text.ICalendar.Types.Properties.Relationship
    ( Attendee(..)
    , Contact(..)
    , Organizer(..)
    , RecurrenceId(..)
    , RelatedTo(..)
    , URL(..)
    , UID(..)
    ) where

import           Data.Set                        (Set)
import           Data.Text.Lazy                  (Text)
import           Data.Typeable                   (Typeable)
import           GHC.Generics                    (Generic)
import           Network.URI                     (URI)

import           Text.ICalendar.Types.Parameters
import           Text.ICalendar.Types.Values


-- | Attendee, 3.8.4.1.
data Attendee = Attendee
    { attendeeValue    :: CalAddress
    , attendeeCUType   :: CUType -- ^ 'def' = 'Individual'
    , attendeeMember   :: Set CalAddress
    , attendeeRole     :: Role -- ^ 'def' = 'ReqParticipant'
    , attendeePartStat :: PartStat -- ^ 'def' = 'PartStatNeedsAction'
    , attendeeRSVP     :: Bool
    , attendeeDelTo    :: Set CalAddress
    , attendeeDelFrom  :: Set CalAddress
    , attendeeSentBy   :: Maybe CalAddress
    , attendeeCN       :: Maybe Text
    , attendeeDir      :: Maybe URI
    , attendeeLanguage :: Maybe Language
    , attendeeOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Contact, 3.8.4.2.
data Contact = Contact
    { contactValue    :: Text
    , contactAltRep   :: Maybe URI
    , contactLanguage :: Maybe Language
    , contactOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Organizer, 3.8.4.3.
data Organizer = Organizer -- TODO: CAL-ADDRESS-related properties.
    { organizerValue    :: CalAddress
    , organizerCN       :: Maybe Text
    , organizerDir      :: Maybe URI
    , organizerSentBy   :: Maybe CalAddress
    , organizerLanguage :: Maybe Language
    , organizerOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Recurrence ID, 3.8.4.4.
data RecurrenceId
    = RecurrenceIdDate
    { recurrenceIdDate  :: Date
    , recurrenceIdRange :: Maybe Range
    , recurrenceIdOther :: OtherParams
    }
    | RecurrenceIdDateTime
    { recurrenceIdDateTime :: DateTime
    , recurrenceIdRange    :: Maybe Range
    , recurrenceIdOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Related To, 3.8.4.5.
data RelatedTo = RelatedTo
    { relatedToValue :: Text
    , relatedToType  :: RelType
    , relatedToOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Uniform Resource Locator, 3.8.4.6.
data URL = URL
    { urlValue :: URI
    , urlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Unique Identifier, 3.8.4.7.
data UID = UID
    { uidValue :: Text
    , uidOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)
