{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- | ICalendar types, based on RFC5545.
module Text.ICalendar.Types where

import Codec.MIME.Type (MIMEType)
import Data.ByteString.Char8 (ByteString)
import Data.CaseInsensitive (CI)
import Data.Default
import Data.Set (Set)
import Data.Text.Lazy (Text)
import Data.Time
import Data.Typeable (Typeable)
import Data.Version (Version(..))
import Network.URI (URI)

type Language = CI Text -- TODO: RFC5646 types and parser.

type CalAddress = URI

-- | One other parameter, either x-param or iana-param.
data OtherParam = OtherParam (CI Text) [Text]
                  deriving (Show, Eq, Ord, Typeable)

-- | Other parameters, either x-param or other iana-param.
data OtherParams = OtherParams (Set OtherParam)
                   deriving (Show, Eq, Ord, Typeable)

instance Default OtherParams where
    def = OtherParams def

-- | VCalendar component, 3.4
data VCalendar = VCalendar
    { vcProdId    :: ProdId
    , vcVersion   :: ICalVersion
    , vcScale     :: Scale
    , vcMethod    :: Maybe Method
    , vcOther     :: OtherParams
    , vcTimeZones :: Set VTimeZone
    , vcEvents    :: Set VEvent
    , vcTodos     :: Set VTodo
    , vcJournal   :: Set VJournal
    , vcFreeBusy  :: Set VFreeBusy
    , vcOtherComp :: Set VOther
    } deriving (Show, Eq, Ord, Typeable)

instance Default VCalendar where
    def = VCalendar (ProdId "-//haskell.org/NONSGML iCalendar-0.1//EN" def)
                    (MaxICalVersion (Version [2,0] []) def)
                    def Nothing def def def def def def def

-- | 3.7.3
data ProdId = ProdId
    { prodIdValue :: Text
    , prodIdOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | 3.7.4
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

-- | 3.7.1
data Scale = Scale 
    { scaleValue :: CI Text
    , scaleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Scale where
    def = Scale "GREGORIAN" def

-- | 3.7.2
data Method = Method
    { methodValue :: CI Text -- TODO: iTIP, RFC5546
    , methodOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data VEvent = VEvent
    { veDTStamp       :: DTStamp
    , veUID           :: UID
    , veDTStart       :: DTStart
    , veClass         :: Class
    , veCreated       :: Maybe Created
    , veDescription   :: Maybe Description
    , veGeo           :: Maybe Geo
    , veLastMod       :: Maybe LastModified
    , veLocation      :: Maybe Location
    , veOrganizer     :: Maybe Organizer
    , vePriority      :: Priority -- def = 0
    , veSeq           :: Sequence -- def = 0
    , veStatus        :: Maybe StatusEvent
    , veSummary       :: Maybe Summary
    , veTransp        :: Transp -- def = Opaque
    , veUrl           :: Maybe URL
    , veRecurId       :: Maybe RecurrenceId
    , veRRule         :: Set RRule
    , veDTEndDuration :: Either DTEnd Duration
    , veAttach        :: Set Attachment
    , veAttendee      :: Set Attendee
    , veCategories    :: Set Category
    , veComment       :: Set Comment
    , veContact       :: Set Contact
    , veExdate        :: Set ExDate
    , veRStatus       :: Set RequestStatus
    , veRelated       :: Set Related
    , veResources     :: Set Resources
    , veRDate         :: Set RDate
    , veAlarms        :: Set VAlarm
    , veOther         :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

data VTodo = VTodo
    { vtDTStamp       :: DTStamp
    , vtUID           :: UID
    , vtClass         :: Class
    , vtCompleted     :: Maybe Completed
    , vtCreated       :: Maybe Created
    , vtDescription   :: Maybe Description
    , vtDTStart       :: Maybe DTStart
    , vtGeo           :: Maybe Geo
    , vtLastMod       :: Maybe LastModified
    , vtLocation      :: Maybe Location
    , vtOrganizer     :: Maybe Organizer
    , vtPercent       :: Maybe Percent
    , vtPriority      :: Priority
    , vtRecurId       :: Maybe RecurrenceId
    , vtSeq           :: Sequence
    , vtStatus        :: Maybe StatusTodo
    , vtSummary       :: Maybe Summary
    , vtUrl           :: Maybe URL
    , vtRRule         :: Set RRule
    , vtDueDuration   :: Either Due Duration
    , vtAttach        :: Set Attachment
    , vtAttendee      :: Set Attendee
    , vtCategories    :: Set Category
    , vtComment       :: Set Comment
    , vtContact       :: Set Contact
    , vtExDate        :: Set ExDate
    , vtRStatus       :: Set RequestStatus
    , vtRelated       :: Set Related
    , vtResources     :: Set Resources
    , vtRDate         :: Set RDate
    , vtAlarms        :: Set VAlarm
    , vtOther         :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

data VJournal = VJournal
    { vjDTStamp       :: DTStamp
    , vjUID           :: UID
    , vjClass         :: Class
    , vjCreated       :: Maybe Created
    , vjDTStart       :: Maybe DTStart
    , vjLastMod       :: Maybe LastModified
    , vjOrganizer     :: Maybe Organizer
    , vjRecurId       :: Maybe RecurrenceId
    , vjSeq           :: Sequence
    , vjStatus        :: Maybe StatusJournal
    , vjSummary       :: Maybe Summary
    , vjUrl           :: Maybe URL
    , vjRRule         :: Set RRule
    , vjAttach        :: Set Attachment
    , vjAttendee      :: Set Attendee
    , vjCategories    :: Set Category
    , vjComment       :: Set Comment
    , vjContact       :: Set Contact
    , vjDescription   :: Set Description
    , vjExDate        :: Set ExDate
    , vjRelated       :: Set Related
    , vjRDate         :: Set RDate
    , vjRStatus       :: Set RequestStatus
    , vjOther         :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

data VFreeBusy = VFreeBusy
    { vfbDTStamp       :: DTStamp
    , vfbUID           :: UID
    , vfbContact       :: Maybe Contact
    , vfbDTStart       :: Maybe DTStart
    , vfbDTEnd         :: Maybe DTEnd
    , vfbOrganizer     :: Maybe Organizer
    , vfbUrl           :: Maybe URL
    , vfbAttendee      :: Set Attendee
    , vfbComment       :: Set Comment
    , vfbFreeBusy      :: Set FreeBusy
    , vfbRStatus       :: Set RequestStatus
    , vfbOther         :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

-- | Time Zone Component. 3.6.5
data VTimeZone = VTimeZone
    { vtzId            :: TZID
    , vtzLastMod       :: Maybe LastModified
    , vtzUrl           :: Maybe TZUrl
    , vtzStandardC     :: Set TZProp
    , vtzDaylightC     :: Set TZProp
    , vtzOther         :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

-- | Time zone property, also 3.6.5
data TZProp = TZProp
    { tzpDTStart      :: DTStart
    , tzpTZOffsetTo   :: UTCOffset
    , tzpTZOffsetFrom :: UTCOffset
    , tzpRRule        :: Set RRule -- Should, but could.
    , tzpComment      :: Set Comment
    , tzpRDate        :: Set RDate
    , tzpTZName       :: Set TZName
    , tzpOther        :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

data VAlarm
    = VAlarmAudio
    { vaTrigger        :: Trigger
    , vaDurationRepeat :: Maybe (Duration, Repeat)
    , vaAudioAttach    :: Maybe Attachment
    , vaOther          :: Set OtherProperty
    }
    | VAlarmDisplay
    { vaDescription    :: Description
    , vaTrigger        :: Trigger
    , vaDurationRepeat :: Maybe (Duration, Repeat)
    , vaOther          :: Set OtherProperty
    }
    | VAlarmEMail
    { vaDescription    :: Description
    , vaTrigger        :: Trigger
    , vaSummary        :: Summary
    , vaAttendee       :: Set Attendee
    , vaDurationRepeat :: Maybe (Duration, Repeat)
    , vaMailAttach     :: Set Attachment
    , vaOther          :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

data VOther = VOther
    { veName :: CI Text
    , veProps :: Set OtherProperty
    } deriving (Show, Eq, Ord, Typeable)

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

data Category = Category
    { categoryValues   :: Set Text
    , categoryLanguage :: Maybe Language
    , categoryOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Class = Class
    { classValue :: ClassValue
    , classOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Completed = Completed
    { completedValue :: DateTime
    , completedOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Class where
    def = Class def def

data ClassValue
    = Public
    | Private
    | Confidential
    | ClassValueX Text
      deriving (Show, Eq, Ord, Typeable)

instance Default ClassValue where
    def = Public

data Comment = Comment
    { commentValue    :: Text
    , commentAltRep   :: Maybe URI
    , commentLanguage :: Maybe Language
    , commentOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Description = Description
    { descriptionValue    :: Text
    , descriptionAltRep   :: Maybe URI
    , descriptionLanguage :: Maybe Language
    , descriptionOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Geo = Geo
    { geoLat   :: Float
    , geoLong  :: Float
    , geoOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Location = Location
    { locationValue    :: Text
    , locationAltRep   :: Maybe URI
    , locationLanguage :: Maybe Language
    , locationOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Percent = Percent
    { percentValue :: Int
    , percentOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Priority = Priority
    { priorityValue :: Int
    , priorityOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Priority where
    def = Priority 0 def

data Resources = Resources
    { resourcesValue    :: Set Text
    , resourcesAltRep   :: Maybe URI
    , resourcesLanguage :: Maybe Language
    , resourcesOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data StatusEvent
    = TentativeEvent
    | ConfirmedEvent
    | CancelledEvent
      deriving (Show, Eq, Ord, Typeable)

data StatusTodo
    = TodoNeedsAction
    | CompletedTodo
    | InProgress
    | CancelledTodo
      deriving (Show, Eq, Ord, Typeable)

data StatusJournal
    = Draft
    | Final
    | CancelledJournal
      deriving (Show, Eq, Ord, Typeable)

data Summary = Summary
    { summaryValue    :: Text
    , summaryAltRep   :: Maybe URI
    , summaryLanguage :: Maybe Language
    , summaryOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Date = Date
    { dateValue :: Day
    } deriving (Show, Eq, Ord, Typeable)

-- | Date-Time value. 3.3.5
data DateTime
    = FloatingDateTime
    { dateTimeFloating :: LocalTime
    }
    | UTCDateTime
    { dateTimeUTC      :: UTCTime
    }
    | ZonedDateTime
    { dateTimeFloating :: LocalTime
    , dateTimeZone     :: Text
    } deriving (Show, Eq, Ord, Typeable)

data DTEnd
    = DTEndDateTime
    { dtEndDateTimeValue :: DateTime
    , dtEndOther         :: OtherParams
    }
    | DTEndDate
    { dtEndDateValue     :: Date
    , dtEndOther         :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Due
    = DueDateTime
    { dueDateTimeValue :: DateTime
    , dueOther         :: OtherParams
    }
    | DueDate
    { dueDateValue     :: Date
    , duether          :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data DTStart
    = DTStartDateTime
    { dtStartDateTimeValue :: DateTime
    , dtStartOther         :: OtherParams
    }
    | DTStartDate
    { dtStartDateValue     :: Date
    , dtStartOther         :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Duration
    = DurationDate
    { durSign   :: Sign
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
    { durSign   :: Sign
    , durWeek   :: Int
    } deriving (Show, Eq, Ord, Typeable)

data Sign = Positive | Negative
            deriving (Show, Eq, Ord, Typeable)

instance Default Sign where
    def = Positive

data FreeBusy = FreeBusy
    { freeBusyType   :: FBType
    , freeBusyPeriod :: Period
    } deriving (Show, Eq, Ord, Typeable)

data Period
    = PeriodDates    UTCTime UTCTime
    | PeriodDuration UTCTime Duration
      deriving (Show, Eq, Ord, Typeable)

data FBType
    = Free
    | Busy
    | BusyUnavailable
    | BusyTentative
    | FBTypeX (CI Text)
      deriving (Show, Eq, Ord, Typeable)

instance Default FBType where
    def = Busy

data Transp
    = Opaque      { transpOther :: OtherParams }
    | Transparent { transpOther :: OtherParams }
      deriving (Show, Eq, Ord, Typeable)

instance Default Transp where
    def = Opaque def

-- | Time Zone Identifier. 3.8.3.1
data TZID = TZID
    { tzidValue  :: Text
    , tzidGlobal :: Bool
    , tzidOther  :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data TZName = TZName
    { tzNameValue    :: Text
    , tzNameLanguage :: Maybe Language
    , tzNameOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | UTC Offset. 3.3.14, 3.8.3.4, and 3.8.3.3 (unified-ish)
data UTCOffset = UTCOffset
    { utcOffsetValue :: Int -- ^ Number of seconds away from UTC
    , utcOffsetOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Time Zone URL. 3.8.3.5
data TZUrl = TZUrl
    { tzUrlValue :: URI
    , tzUrlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)


data Attendee = Attendee
    { attendeeValue    :: CalAddress
    , attendeeCUType   :: CUType
    , attendeeMember   :: Set CalAddress
    , attendeeRole     :: Role
    , attendeePartStat :: PartStat
    , attendeeRSVP     :: Bool
    , attendeeDelTo    :: Set CalAddress
    , attendeeDelFrom  :: Set CalAddress
    , attendeeSentBy   :: Maybe CalAddress
    , attendeeCN       :: Maybe Text
    , attendeeDir      :: Maybe URI
    , attendeeLanguage :: Maybe Language
    , attendeeOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

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

data Role = Chair
          | ReqParticipant
          | OptParticipant
          | NonParticipant
          | RoleX (CI Text)
            deriving (Show, Eq, Ord, Typeable)

instance Default Role where
    def = ReqParticipant

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

data Contact = Contact
    { contactValue    :: Text
    , contactAltRep   :: Maybe URI
    , contactLanguage :: Maybe Language
    , contactOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Organizer = Organizer
    { organizerValue    :: CalAddress
    , organiserCN       :: Maybe Text
    , organizerDir      :: Maybe URI
    , organizerSentBy   :: Maybe CalAddress
    , organizerLanguage :: Maybe Language
    , organizerOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data RecurrenceId
    = RecurrenceIdDate
    { recurrenceIdDate  :: Date
    , recurrenceIdOther :: OtherParams
    }
    | RecurrenceIdDateTime
    { recurrenceIdDateTime :: DateTime
    , recurrenceIdOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Related = Related
    { relatedValue :: UID
    , relatedType  :: Relation
    } deriving (Show, Eq, Ord, Typeable)

data Relation = Parent | Sibling | Child | OtherRelation Text
                deriving (Show, Eq, Ord, Typeable)

instance Default Relation where
    def = Parent

data URL = URL
    { urlValue :: URI
    , urlOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data UID = UID
    { uidValue :: Text
    , uidOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data ExDate
    = ExDate
    { exDate  :: Date
    , exOther :: OtherParams
    }
    | ExDateTime
    { exDateTime :: DateTime
    , exOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data RDate
    = RDateDate
    { rDateDate  :: Date
    , rDateOther :: OtherParams
    }
    | RDateDateTime
    { rDateDateTime :: DateTime
    , rDateOther    :: OtherParams
    }
    | RDatePeriod
    { rDatePeriod :: Period
    , rDateOther  :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Frequency
    = Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
      deriving (Show, Eq, Ord, Typeable)

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday
                      | Friday | Saturday
               deriving (Show, Eq, Ord, Bounded, Enum, Typeable)

-- | Recur value. 3.3.10
data Recur = Recur
    { recurFreq       :: Frequency
    , recurUntilCount :: Either (Either Date DateTime) Int
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

data UntilCount
    = UntilCountDate
    { untilCountDateValue :: Date
    , untilCountOther     :: OtherParams
    }
    | UntilCountDateTime
    { untilCountDateTimeValue :: DateTime
    , untilCountOther         :: OtherParams
    }
    | UntilCount
    { untilCountValue :: Int
    , untilCountOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Recurrence Rule. 3.8.5.3
data RRule = RRule
    { rRuleValue :: Recur
    , rRuleOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Repeat = Repeat
    { repeatValue :: Integer
    , repeatOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Repeat where
    def = Repeat 0 def

data DurRelated = Start | End
                  deriving (Show, Eq, Ord, Typeable)

instance Default DurRelated where
    def = Start

data Trigger
    = TriggerDuration
    { triggerDuration :: Duration
    , triggerRelated  :: DurRelated
    , triggerOther    :: OtherParams
    }
    | TriggerDateTime
    { triggerDateTime :: DateTime
    , triggerOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Created = Created
    { createdValue :: DateTime
    , createdOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data DTStamp = DTStamp
    { dtStampValue :: DateTime
    , dtStampOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

-- | Last Modified. 3.8.7.3
data LastModified = LastModified
    { lastModifiedValue :: UTCTime
    , lastModifiedOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data Sequence = Sequence
    { sequenceValue :: Integer
    , sequenceOther :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

instance Default Sequence where
    def = Sequence 0 def

data RequestStatus = RequestStatus
    { requestStatusCode     :: [Int]
    , requestStatusDesc     :: Text
    , requestStatusLanguage :: Maybe Language
    , requestStatusExt      :: Maybe Text
    , requestStatusOther    :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)

data OtherProperty = OtherProperty
    { otherValue  :: ByteString
    , otherParams :: OtherParams
    } deriving (Show, Eq, Ord, Typeable)
