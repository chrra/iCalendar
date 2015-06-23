{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | ICalendar types, based on RFC5545.
module Text.ICalendar.Types
    ( module Text.ICalendar.Types
    , module Text.ICalendar.Types.Components
    , module Text.ICalendar.Types.Parameters
    , module Text.ICalendar.Types.Properties.Alarm
    , module Text.ICalendar.Types.Properties.Calendar
    , module Text.ICalendar.Types.Properties.ChangeManagement
    , module Text.ICalendar.Types.Properties.DateTime
    , module Text.ICalendar.Types.Properties.Descriptive
    , module Text.ICalendar.Types.Properties.Misc
    , module Text.ICalendar.Types.Properties.Recurrence
    , module Text.ICalendar.Types.Properties.Relationship
    , module Text.ICalendar.Types.Properties.TimeZone
    , module Text.ICalendar.Types.Values
    ) where

import           Codec.MIME.Type                                  (MIMEType)
import           Data.ByteString.Lazy.Char8                       (ByteString)
import           Data.CaseInsensitive                             (CI)
import           Data.Default
import           Data.Map                                         (Map)
import qualified Data.Map                                         as M
import           Data.Monoid
import           Data.Set                                         (Set)
import           Data.Text.Lazy                                   (Text, pack)
import           Data.Time
import           Data.Typeable                                    (Typeable)
import           Data.Version                                     (Version (..),
                                                                   showVersion)
import           GHC.Generics                                     (Generic)
import           Network.URI                                      (URI)

import           Text.ICalendar.Types.Components
import           Text.ICalendar.Types.Parameters
import           Text.ICalendar.Types.Properties.Alarm
import           Text.ICalendar.Types.Properties.Calendar
import           Text.ICalendar.Types.Properties.ChangeManagement
import           Text.ICalendar.Types.Properties.DateTime
import           Text.ICalendar.Types.Properties.Descriptive
import           Text.ICalendar.Types.Properties.Misc
import           Text.ICalendar.Types.Properties.Recurrence
import           Text.ICalendar.Types.Properties.Relationship
import           Text.ICalendar.Types.Properties.TimeZone
import           Text.ICalendar.Types.Values

import           Paths_iCalendar                                  (version)

-- | VCalendar component. 3.4.
data VCalendar = VCalendar
    { vcProdId     :: ProdId
    , vcVersion    :: ICalVersion
    , vcScale      :: Scale
    , vcMethod     :: Maybe Method
    , vcOther      :: Set OtherProperty
    , vcTimeZones  :: Map Text VTimeZone
    -- ^ Map TZID-value VTimeZone
    , vcEvents     :: Map (Text, Maybe (Either Date DateTime)) VEvent
    -- ^ Map (UID-value, Maybe RecurrenceID-value) VEvent
    , vcTodos      :: Map (Text, Maybe (Either Date DateTime)) VTodo
    -- ^ Map (UID-value, Maybe RecurrenceID-value) VTodo
    , vcJournals   :: Map (Text, Maybe (Either Date DateTime)) VJournal
    -- ^ Map (UID-value, Maybe RecurrenceID-value) VJournal
    , vcFreeBusys  :: Map Text VFreeBusy
    -- ^ Map UID-value VFreeBusy
    , vcOtherComps :: Set VOther
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default VCalendar where
    def = VCalendar (ProdId ("-//haskell.org/NONSGML iCalendar-" <>
                             pack (showVersion version) <> "//EN") def)
                    (MaxICalVersion (Version [2,0] []) def)
                    def Nothing def def def def def def def

-- | 'vcMethod' is ignored at the moment.
--
-- Picks the left in most cases.
--
-- On UID/RecurrenceId/TZID clash, picks the 'VEvent's, 'VTodo's and
-- 'VJournal's with the highest ('Sequence', 'DTStamp'), the 'VTimeZone's
-- with the highest 'LastModified', and 'VFreeBusy' with the highest 'DTStamp'.
--
-- If the Sequence, DTStamp or LastModified is the same, picks the left.
instance Monoid VCalendar where
    mempty = def
    mappend a b = VCalendar { vcProdId     = vcProdId a
                            , vcVersion    = vcVersion a
                            , vcScale      = vcScale a
                            , vcMethod     = vcMethod a
                            , vcOther      = vcOther a <> vcOther b
                            , vcTimeZones  = merge tz (vcTimeZones a)
                                                      (vcTimeZones b)
                            , vcEvents     = merge ev (vcEvents a) (vcEvents b)
                            , vcTodos      = merge td (vcTodos a) (vcTodos b)
                            , vcJournals   = merge jo (vcJournals a)
                                                      (vcJournals b)
                            , vcFreeBusys  = merge fb (vcFreeBusys a)
                                                      (vcFreeBusys b)
                            , vcOtherComps = vcOtherComps a <> vcOtherComps b
                            }
      where merge f = M.mergeWithKey (((Just .) .) . const f) id id
            tz c d = if vtzLastMod c >= vtzLastMod d then c else d
            ev c d = if (veSeq c, veDTStamp c) >= (veSeq d, veDTStamp d)
                        then c else d
            td c d = if (vtSeq c, vtDTStamp c) >= (vtSeq d, vtDTStamp d)
                        then c else d
            jo c d = if (vjSeq c, vjDTStamp c) >= (vjSeq d, vjDTStamp d)
                        then c else d
            fb c d = if vfbDTStamp c >= vfbDTStamp d then c else d

-- | Product Identifier. 3.7.3.
data ProdId = ProdId
    { prodIdValue :: Text
    , prodIdOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Version. 3.7.4.
data ICalVersion
    = MaxICalVersion
    { versionMax   :: Version
    , versionOther :: OtherParams
    }
    | MinMaxICalVersion
    { versionMax   :: Version
    , versionMin   :: Version
    , versionOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Calendar Scale. 3.7.1.
data Scale = Scale
    { scaleValue :: CI Text
    , scaleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Scale where
    def = Scale "GREGORIAN" def

-- | Method. 3.7.2.
data Method = Method
    { methodValue :: CI Text -- TODO: iTIP, RFC5546
    , methodOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Event Component. 3.6.1.
data VEvent = VEvent
    { veDTStamp       :: DTStamp
    , veUID           :: UID
    , veClass         :: Class -- ^ 'def' = 'Public'
    , veDTStart       :: Maybe DTStart
    , veCreated       :: Maybe Created
    , veDescription   :: Maybe Description
    , veGeo           :: Maybe Geo
    , veLastMod       :: Maybe LastModified
    , veLocation      :: Maybe Location
    , veOrganizer     :: Maybe Organizer
    , vePriority      :: Priority -- ^ 'def' = 0
    , veSeq           :: Sequence -- ^ 'def' = 0
    , veStatus        :: Maybe EventStatus
    , veSummary       :: Maybe Summary
    , veTransp        :: Transp -- ^ 'def' = 'Opaque'
    , veUrl           :: Maybe URL
    , veRecurId       :: Maybe RecurrenceId
    , veRRule         :: Set RRule
    , veDTEndDuration :: Maybe (Either DTEnd DurationProp)
    , veAttach        :: Set Attach
    , veAttendee      :: Set Attendee
    , veCategories    :: Set Categories
    , veComment       :: Set Comment
    , veContact       :: Set Contact
    , veExDate        :: Set ExDate
    , veRStatus       :: Set RequestStatus
    , veRelated       :: Set RelatedTo
    , veResources     :: Set Resources
    , veRDate         :: Set RDate
    , veAlarms        :: Set VAlarm
    , veOther         :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | To-Do Component. 3.6.2
data VTodo = VTodo
    { vtDTStamp     :: DTStamp
    , vtUID         :: UID
    , vtClass       :: Class -- ^ 'def' = 'Public'
    , vtCompleted   :: Maybe Completed
    , vtCreated     :: Maybe Created
    , vtDescription :: Maybe Description
    , vtDTStart     :: Maybe DTStart
    , vtGeo         :: Maybe Geo
    , vtLastMod     :: Maybe LastModified
    , vtLocation    :: Maybe Location
    , vtOrganizer   :: Maybe Organizer
    , vtPercent     :: Maybe PercentComplete
    , vtPriority    :: Priority -- ^ 'def' = 0
    , vtRecurId     :: Maybe RecurrenceId
    , vtSeq         :: Sequence -- ^ 'def' = 0
    , vtStatus      :: Maybe TodoStatus
    , vtSummary     :: Maybe Summary
    , vtUrl         :: Maybe URL
    , vtRRule       :: Set RRule
    , vtDueDuration :: Maybe (Either Due DurationProp)
    , vtAttach      :: Set Attach
    , vtAttendee    :: Set Attendee
    , vtCategories  :: Set Categories
    , vtComment     :: Set Comment
    , vtContact     :: Set Contact
    , vtExDate      :: Set ExDate
    , vtRStatus     :: Set RequestStatus
    , vtRelated     :: Set RelatedTo
    , vtResources   :: Set Resources
    , vtRDate       :: Set RDate
    , vtAlarms      :: Set VAlarm
    , vtOther       :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Journal Component. 3.6.3
data VJournal = VJournal
    { vjDTStamp     :: DTStamp
    , vjUID         :: UID
    , vjClass       :: Class -- ^ 'def' = 'Public'
    , vjCreated     :: Maybe Created
    , vjDTStart     :: Maybe DTStart
    , vjLastMod     :: Maybe LastModified
    , vjOrganizer   :: Maybe Organizer
    , vjRecurId     :: Maybe RecurrenceId
    , vjSeq         :: Sequence -- ^ 'def' = 0
    , vjStatus      :: Maybe JournalStatus
    , vjSummary     :: Maybe Summary
    , vjUrl         :: Maybe URL
    , vjRRule       :: Set RRule
    , vjAttach      :: Set Attach
    , vjAttendee    :: Set Attendee
    , vjCategories  :: Set Categories
    , vjComment     :: Set Comment
    , vjContact     :: Set Contact
    , vjDescription :: Set Description
    , vjExDate      :: Set ExDate
    , vjRelated     :: Set RelatedTo
    , vjRDate       :: Set RDate
    , vjRStatus     :: Set RequestStatus
    , vjOther       :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Free/Busy Component. 3.6.4
data VFreeBusy = VFreeBusy
    { vfbDTStamp   :: DTStamp
    , vfbUID       :: UID
    , vfbContact   :: Maybe Contact
    , vfbDTStart   :: Maybe DTStart
    , vfbDTEnd     :: Maybe DTEnd
    , vfbOrganizer :: Maybe Organizer
    , vfbUrl       :: Maybe URL
    , vfbAttendee  :: Set Attendee
    , vfbComment   :: Set Comment
    , vfbFreeBusy  :: Set FreeBusy
    , vfbRStatus   :: Set RequestStatus
    , vfbOther     :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Time Zone Component. 3.6.5.
data VTimeZone = VTimeZone
    { vtzId        :: TZID
    , vtzLastMod   :: Maybe LastModified
    , vtzUrl       :: Maybe TZUrl
    , vtzStandardC :: Set TZProp
    , vtzDaylightC :: Set TZProp
    , vtzOther     :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Time zone property, also 3.6.5.
data TZProp = TZProp
    { tzpDTStart      :: DTStart
    , tzpTZOffsetTo   :: UTCOffset
    , tzpTZOffsetFrom :: UTCOffset
    , tzpRRule        :: Set RRule -- SHOULD NOT have multiple RRules.
    , tzpComment      :: Set Comment
    , tzpRDate        :: Set RDate
    , tzpTZName       :: Set TZName
    , tzpOther        :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | VAlarm component. 3.6.6.
data VAlarm
    = VAlarmAudio
    { vaTrigger     :: Trigger
    , vaRepeat      :: Repeat -- ^ 'def' = 0
    , vaDuration    :: Maybe DurationProp
    , vaAudioAttach :: Maybe Attach
    , vaOther       :: Set OtherProperty
    , vaActionOther :: OtherParams
    }
    | VAlarmDisplay
    { vaDescription :: Description
    , vaTrigger     :: Trigger
    , vaRepeat      :: Repeat
    , vaDuration    :: Maybe DurationProp
    , vaOther       :: Set OtherProperty
    , vaActionOther :: OtherParams
    }
    | VAlarmEmail
    { vaDescription :: Description
    , vaTrigger     :: Trigger
    , vaSummary     :: Summary
    , vaAttendee    :: Set Attendee
    , vaRepeat      :: Repeat
    , vaDuration    :: Maybe DurationProp
    , vaMailAttach  :: Set Attach
    , vaOther       :: Set OtherProperty
    , vaActionOther :: OtherParams
    }
    | VAlarmX
    { vaAction      :: CI Text
    , vaTrigger     :: Trigger
    , vaActionOther :: OtherParams
    , vaOther       :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Any other component not recognized.
data VOther = VOther
    { voName  :: CI Text
    , voProps :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable, Generic)


-- | Time Zone Identifier. 3.8.3.1.
data TZID = TZID
    { tzidValue  :: Text -- ^ Full name, including solidus if present.
    , tzidGlobal :: Bool -- ^ If the solidus, indicating globalness, is present.
    , tzidOther  :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Time Zone Name. 3.8.3.2.
data TZName = TZName
    { tzNameValue    :: Text
    , tzNameLanguage :: Maybe Language
    , tzNameOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Time Zone URL. 3.8.3.5.
data TZUrl = TZUrl
    { tzUrlValue :: URI
    , tzUrlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Attendee. 3.8.4.1.
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

-- | Contact. 3.8.4.2.
data Contact = Contact
    { contactValue    :: Text
    , contactAltRep   :: Maybe URI
    , contactLanguage :: Maybe Language
    , contactOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Organizer. 3.8.4.3.
--
-- TODO: CAL-ADDRESS-related properties.
data Organizer = Organizer
    { organizerValue    :: CalAddress
    , organizerCN       :: Maybe Text
    , organizerDir      :: Maybe URI
    , organizerSentBy   :: Maybe CalAddress
    , organizerLanguage :: Maybe Language
    , organizerOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Recurrence ID. 3.8.4.4.
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

-- | Related To. 3.8.4.5.
data RelatedTo = RelatedTo
    { relatedToValue :: Text
    , relatedToType  :: RelType
    , relatedToOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Uniform Resource Locator. 3.8.4.6.
data URL = URL
    { urlValue :: URI
    , urlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Unique Identifier. 3.8.4.7.
data UID = UID
    { uidValue :: Text
    , uidOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Exception Date-Times. 3.8.5.1.
data ExDate
    = ExDates
    { exDates     :: Set Date
    , exDateOther :: OtherParams
    }
    | ExDateTimes
    { exDateTimes :: Set DateTime
    , exDateOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Recurrence Date-Times. 3.8.5.2.
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


-- | Recurrence Rule. 3.8.5.3.
data RRule = RRule
    { rRuleValue :: Recur
    , rRuleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Repeat count. 3.8.6.2.
data Repeat = Repeat
    { repeatValue :: Integer
    , repeatOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Repeat where
    def = Repeat 0 def

-- | Trigger. 3.8.6.3.
data Trigger
    = TriggerDuration
    { triggerDuration :: Duration
    , triggerRelated  :: Related -- ^ 'def' = 'Start'
    , triggerOther    :: OtherParams
    }
    | TriggerDateTime
    { triggerDateTime :: UTCTime
    , triggerOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

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

-- | Request Status. 3.8.8.3.
data RequestStatus = RequestStatus
    { requestStatusCode     :: [Int]
    , requestStatusDesc     :: Text
    , requestStatusLanguage :: Maybe Language
    , requestStatusExt      :: Maybe Text
    , requestStatusOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Any other property.
data OtherProperty = OtherProperty
    { otherName   :: CI Text
    , otherValue  :: ByteString
    , otherParams :: OtherParams
    } deriving (Show, Eq, Ord, Typeable, Generic)
