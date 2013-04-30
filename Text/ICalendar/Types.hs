{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | ICalendar types, based on RFC5545.
module Text.ICalendar.Types
    ( module Text.ICalendar.Types
    ) where

import           Codec.MIME.Type            (MIMEType)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.CaseInsensitive       (CI)
import           Data.Default
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Monoid
import           Data.Set                   (Set)
import           Data.Text.Lazy             (Text, pack)
import           Data.Time
import           Data.Typeable              (Typeable)
import           Data.Version               (Version (..), showVersion)
import           Network.URI                (URI)

import Paths_iCalendar (version)

-- | Language.
newtype Language = Language (CI Text) -- TODO: RFC5646 types and parser.
                   deriving (Eq, Show, Ord, Typeable)

type CalAddress = URI

-- | One other parameter, either x-param or iana-param.
data OtherParam = OtherParam (CI Text) [Text]
                  deriving (Show, Eq, Ord, Typeable)

-- | Other parameters, either x-param or other iana-param.
data OtherParams = OtherParams (Set OtherParam)
                   deriving (Show, Eq, Ord, Typeable)

instance Default OtherParams where
    def = OtherParams def

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
    } deriving (Show, Eq, Ord, Typeable)

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
    } deriving (Show, Eq, Ord, Typeable)

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
    } deriving (Show, Eq, Ord, Typeable)

-- | Calendar Scale. 3.7.1.
data Scale = Scale
    { scaleValue :: CI Text
    , scaleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Scale where
    def = Scale "GREGORIAN" def

-- | Method. 3.7.2.
data Method = Method
    { methodValue :: CI Text -- TODO: iTIP, RFC5546
    , methodOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

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
    , veTransp        :: TimeTransparency -- ^ 'def' = 'Opaque'
    , veUrl           :: Maybe URL
    , veRecurId       :: Maybe RecurrenceId
    , veRRule         :: Set RRule
    , veDTEndDuration :: Maybe (Either DTEnd DurationProp)
    , veAttach        :: Set Attachment
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
    } deriving (Show, Eq, Ord, Typeable)

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
    , vtAttach      :: Set Attachment
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
    } deriving (Show, Eq, Ord, Typeable)

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
    , vjAttach      :: Set Attachment
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
    } deriving (Show, Eq, Ord, Typeable)

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
    } deriving (Show, Eq, Ord, Typeable)

-- | Time Zone Component. 3.6.5.
data VTimeZone = VTimeZone
    { vtzId        :: TZID
    , vtzLastMod   :: Maybe LastModified
    , vtzUrl       :: Maybe TZUrl
    , vtzStandardC :: Set TZProp
    , vtzDaylightC :: Set TZProp
    , vtzOther     :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

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
    } deriving (Show, Eq, Ord, Typeable)

-- | VAlarm component. 3.6.6.
data VAlarm
    = VAlarmAudio
    { vaTrigger     :: Trigger
    , vaRepeat      :: Repeat -- ^ 'def' = 0
    , vaDuration    :: Maybe DurationProp
    , vaAudioAttach :: Maybe Attachment
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
    , vaMailAttach  :: Set Attachment
    , vaOther       :: Set OtherProperty
    , vaActionOther :: OtherParams
    }
    | VAlarmX
    { vaAction      :: CI Text
    , vaTrigger     :: Trigger
    , vaActionOther :: OtherParams
    , vaOther       :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

-- | Any other component not recognized.
data VOther = VOther
    { voName  :: CI Text
    , voProps :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

-- | Attachment. 3.8.1.1.
data Attachment
    = UriAttachment
    { attachFmtType :: Maybe MIMEType
    , attachUri     :: URI
    , attachOther   :: OtherParams
    }
    | BinaryAttachment
    { attachFmtType :: Maybe MIMEType
    , attachContent :: ByteString
    , attachOther   :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Categories. 3.8.1.2.
data Categories = Categories
    { categoriesValues   :: Set Text
    , categoriesLanguage :: Maybe Language
    , categoriesOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Classification. 3.8.1.3.
data Class = Class
    { classValue :: ClassValue
    , classOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Class where
    def = Class def def

-- | Classification value. 3.8.1.3.
-- Unrecognized ClassValueX MUST be treated as Private.
data ClassValue
    = Public
    | Private
    | Confidential
    | ClassValueX (CI Text)
      deriving (Show, Eq, Ord, Typeable)

instance Default ClassValue where
    def = Public

-- | Date-Time Completed. 3.8.2.1.
data Completed = Completed
    { completedValue :: DateTime
    , completedOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Comment. 3.8.1.4.
data Comment = Comment
    { commentValue    :: Text
    , commentAltRep   :: Maybe URI
    , commentLanguage :: Maybe Language
    , commentOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Description. 3.8.1.5.
data Description = Description
    { descriptionValue    :: Text
    , descriptionAltRep   :: Maybe URI
    , descriptionLanguage :: Maybe Language
    , descriptionOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Geographic Position. 3.8.1.6.
data Geo = Geo
    { geoLat   :: Float
    , geoLong  :: Float
    , geoOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Location. 3.8.1.7.
data Location = Location
    { locationValue    :: Text
    , locationAltRep   :: Maybe URI
    , locationLanguage :: Maybe Language
    , locationOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Percent complete. 3.8.1.8.
data PercentComplete = PercentComplete
    { percentCompleteValue :: Int
    , percentCompleteOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Priority. 3.8.1.9.
data Priority = Priority
    { priorityValue :: Int
    , priorityOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Priority where
    def = Priority 0 def

-- | Resources. 3.8.1.10.
data Resources = Resources
    { resourcesValue    :: Set Text
    , resourcesAltRep   :: Maybe URI
    , resourcesLanguage :: Maybe Language
    , resourcesOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Status, but only for Events. 3.8.1.11.
data EventStatus
    = TentativeEvent { eventStatusOther :: OtherParams }
    | ConfirmedEvent { eventStatusOther :: OtherParams }
    | CancelledEvent { eventStatusOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable)

-- | Status, but only for TODOs. 3.8.1.11.
data TodoStatus
    = TodoNeedsAction { todoStatusOther :: OtherParams }
    | CompletedTodo   { todoStatusOther :: OtherParams }
    | InProcessTodo   { todoStatusOther :: OtherParams }
    | CancelledTodo   { todoStatusOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable)

-- | Status, but only for Journals. 3.8.1.11.
data JournalStatus
    = DraftJournal     { journalStatusOther :: OtherParams }
    | FinalJournal     { journalStatusOther :: OtherParams }
    | CancelledJournal { journalStatusOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable)

-- | Summary. 3.8.1.12.
data Summary = Summary
    { summaryValue    :: Text
    , summaryAltRep   :: Maybe URI
    , summaryLanguage :: Maybe Language
    , summaryOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Date. 3.3.4
data Date = Date
    { dateValue :: Day
    } deriving (Show, Eq, Ord, Typeable)

-- | Date-Time value. 3.3.5.
data DateTime
    = FloatingDateTime
    { dateTimeFloating :: LocalTime
    }
    | UTCDateTime
    { dateTimeUTC :: UTCTime
    }
    | ZonedDateTime
    { dateTimeFloating :: LocalTime
    , dateTimeZone     :: Text
    } deriving (Show, Eq, Ord, Typeable)

-- | Date-Time End. 3.8.2.2.
data DTEnd
    = DTEndDateTime
    { dtEndDateTimeValue :: DateTime
    , dtEndOther         :: OtherParams
    }
    | DTEndDate
    { dtEndDateValue :: Date
    , dtEndOther     :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Date-Time Due. 3.8.2.3.
data Due
    = DueDateTime
    { dueDateTimeValue :: DateTime
    , dueOther         :: OtherParams
    }
    | DueDate
    { dueDateValue :: Date
    , dueOther     :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Date-Time Start. 3.8.2.4.
data DTStart
    = DTStartDateTime
    { dtStartDateTimeValue :: DateTime
    , dtStartOther         :: OtherParams
    }
    | DTStartDate
    { dtStartDateValue :: Date
    , dtStartOther     :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Duration value. 3.3.6.
data Duration -- TODO(?): Convert to DiffTime?
    = DurationDate
    { durSign   :: Sign -- ^ 'def' = 'Positive'
    , durDay    :: Int
    , durHour   :: Int
    , durMinute :: Int
    , durSecond :: Int
    }
    | DurationTime
    { durSign   :: Sign
    , durHour   :: Int
    , durMinute :: Int
    , durSecond :: Int
    }
    | DurationWeek
    { durSign :: Sign
    , durWeek :: Int
    } deriving (Show, Eq, Ord, Typeable)

-- | Sign.
data Sign = Positive | Negative
            deriving (Show, Eq, Ord, Typeable)

instance Default Sign where
    def = Positive

-- | Duration property. 3.8.2.5.
data DurationProp = DurationProp
    { durationValue :: Duration
    , durationOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data FreeBusy = FreeBusy
    { freeBusyType    :: FBType
    , freeBusyPeriods :: Set UTCPeriod
    , freeBusyOther   :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Period of time. 3.3.9.
data Period
    = PeriodDates    DateTime DateTime
    | PeriodDuration DateTime Duration
      deriving (Show, Eq, Ord, Typeable)

-- | Period of time which must be UTC, as in FreeBusy. 3.3.9.
data UTCPeriod
    = UTCPeriodDates    UTCTime UTCTime
    | UTCPeriodDuration UTCTime Duration
      deriving (Show, Eq, Ord, Typeable)

-- | Free/Busy Time Type. 3.2.9.
-- Unrecognized FBTypeX MUST be treated as Busy.
data FBType
    = Free
    | Busy
    | BusyUnavailable
    | BusyTentative
    | FBTypeX (CI Text)
      deriving (Show, Eq, Ord, Typeable)

instance Default FBType where
    def = Busy

-- | Time Transparency. 3.8.2.7.
data TimeTransparency
    = Opaque      { timeTransparencyOther :: OtherParams }
    | Transparent { timeTransparencyOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable)

instance Default TimeTransparency where
    def = Opaque def

-- | Time Zone Identifier. 3.8.3.1.
data TZID = TZID
    { tzidValue  :: Text
    , tzidGlobal :: Bool
    , tzidOther  :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Time Zone Name. 3.8.3.2.
data TZName = TZName
    { tzNameValue    :: Text
    , tzNameLanguage :: Maybe Language
    , tzNameOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | UTC Offset. 3.3.14, 3.8.3.4, and 3.8.3.3. (unified-ish)
data UTCOffset = UTCOffset
    { utcOffsetValue :: Int -- ^ Number of seconds away from UTC
    , utcOffsetOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Time Zone URL. 3.8.3.5.
data TZUrl = TZUrl
    { tzUrlValue :: URI
    , tzUrlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

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
    } deriving (Show, Eq, Ord, Typeable)

-- | Calendar User Type. 3.2.3.
-- Unrecognized CUTypeX MUST be treated as Unknown.
data CUType
    = Individual
    | Group
    | Resource
    | Room
    | Unknown
    | CUTypeX (CI Text)
      deriving (Show, Eq, Ord, Typeable)

instance Default CUType where
    def = Individual

-- | Role. 3.2.16.
data Role = Chair
          | ReqParticipant
          | OptParticipant
          | NonParticipant
          | RoleX (CI Text)
            deriving (Show, Eq, Ord, Typeable)

instance Default Role where
    def = ReqParticipant

-- | Participation Status. 3.2.12.
data PartStat -- Splitting requires splitting attendee too...
    = PartStatNeedsAction
    | Accepted
    | Declined
    | Tentative
    | Delegated
    | PartStatCompleted
    | InProcess
    | PartStatX (CI Text)
      deriving (Show, Eq, Ord, Typeable)

instance Default PartStat where
    def = PartStatNeedsAction

-- | Contact. 3.8.4.2.
data Contact = Contact
    { contactValue    :: Text
    , contactAltRep   :: Maybe URI
    , contactLanguage :: Maybe Language
    , contactOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Organizer. 3.8.4.3.
-- TODO: CAL-ADDRESS-related properties.
data Organizer = Organizer
    { organizerValue    :: CalAddress
    , organizerCN       :: Maybe Text
    , organizerDir      :: Maybe URI
    , organizerSentBy   :: Maybe CalAddress
    , organizerLanguage :: Maybe Language
    , organizerOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

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
    } deriving (Show, Eq, Ord, Typeable)

-- | Recurrence Identifier Range. 3.2.13
data Range = ThisAndFuture | ThisAndPrior
             deriving (Show, Eq, Ord, Typeable)

-- | Related To. 3.8.4.5.
data RelatedTo = RelatedTo
    { relatedToValue :: Text
    , relatedToType  :: RelationshipType
    , relatedToOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Relationship Type. 3.2.15.
-- Unrecognized RelationshipTypeX values MUST be treated as Parent.
data RelationshipType = Parent | Child | Sibling | RelationshipTypeX (CI Text)
                        deriving (Show, Eq, Ord, Typeable)

instance Default RelationshipType where
    def = Parent

-- | Uniform Resource Locator. 3.8.4.6.
data URL = URL
    { urlValue :: URI
    , urlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Unique Identifier. 3.8.4.7.
data UID = UID
    { uidValue :: Text
    , uidOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Exception Date-Times. 3.8.5.1.
data ExDate
    = ExDates
    { exDates     :: Set Date
    , exDateOther :: OtherParams
    }
    | ExDateTimes
    { exDateTimes :: Set DateTime
    , exDateOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

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
    } deriving (Show, Eq, Ord, Typeable)

-- | Frequency in recurrences. 3.3.10.
data Frequency
    = Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
      deriving (Show, Eq, Ord, Typeable)

-- | Weekday, in recurrences. 3.3.10.
data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday
                      | Friday | Saturday
               deriving (Show, Eq, Ord, Bounded, Enum, Typeable)

-- | Recur value. 3.3.10.
data Recur = Recur
    { recurFreq       :: Frequency
    , recurUntilCount :: Maybe (Either (Either Date DateTime) Int)
    , recurInterval   :: Int
    , recurBySecond   :: [Int]
    , recurByMinute   :: [Int]
    , recurByHour     :: [Int]
    , recurByDay      :: [Either (Int, Weekday) Weekday]
    , recurByMonthDay :: [Int]
    , recurByYearDay  :: [Int]
    , recurByWeekNo   :: [Int]
    , recurByMonth    :: [Int]
    , recurBySetPos   :: [Int]
    , recurWkSt       :: Weekday
    } deriving (Show, Eq, Ord, Typeable)

-- | Recurrence Rule. 3.8.5.3.
data RRule = RRule
    { rRuleValue :: Recur
    , rRuleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Repeat count. 3.8.6.2.
data Repeat = Repeat
    { repeatValue :: Integer
    , repeatOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Repeat where
    def = Repeat 0 def

-- | Alarm Trigger Relationship. 3.2.14.
data AlarmTriggerRelationship = Start | End
                                deriving (Show, Eq, Ord, Typeable)

instance Default AlarmTriggerRelationship where
    def = Start

-- | Trigger. 3.8.6.3.
data Trigger
    = TriggerDuration
    { triggerDuration :: Duration
    , triggerRelated  :: AlarmTriggerRelationship -- ^ 'def' = 'Start'
    , triggerOther    :: OtherParams
    }
    | TriggerDateTime
    { triggerDateTime :: UTCTime
    , triggerOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Date-Time Created. 3.8.7.1.
data Created = Created
    { createdValue :: UTCTime
    , createdOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Date-Time Stamp. 3.8.7.2.
data DTStamp = DTStamp
    { dtStampValue :: UTCTime
    , dtStampOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Last Modified. 3.8.7.3.
data LastModified = LastModified
    { lastModifiedValue :: UTCTime
    , lastModifiedOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Sequence number. 3.8.7.4.
data Sequence = Sequence
    { sequenceValue :: Integer
    , sequenceOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Sequence where
    def = Sequence 0 def

-- | Request Status. 3.8.8.3.
data RequestStatus = RequestStatus
    { requestStatusCode     :: [Int]
    , requestStatusDesc     :: Text
    , requestStatusLanguage :: Maybe Language
    , requestStatusExt      :: Maybe Text
    , requestStatusOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Any other property.
data OtherProperty = OtherProperty
    { otherName   :: CI Text
    , otherValue  :: ByteString
    , otherParams :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)
