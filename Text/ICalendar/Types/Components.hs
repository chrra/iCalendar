{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Calendar Components
--
-- <https://tools.ietf.org/html/rfc5545#section-3.6>
module Text.ICalendar.Types.Components
    ( VCalendar(..)
    , VEvent(..)
    , VTodo(..)
    , VJournal(..)
    , VFreeBusy(..)
    , VTimeZone(..)
    , TZProp(..)
    , VAlarm(..)
    , VOther(..)
    ) where

import           Data.CaseInsensitive                             (CI)
import           Data.Default
import           Data.Map                                         (Map)
import qualified Data.Map                                         as M
import           Data.Monoid
import           Data.Set                                         (Set)
import           Data.Text.Lazy                                   (Text, pack)
import           Data.Typeable                                    (Typeable)
import qualified Data.Version                                     as V
import           GHC.Generics                                     (Generic)

import           Paths_iCalendar                                  (version)
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
import           Text.ICalendar.Types.Parameters

-- | ICalendar object, 3.4, 3.6.
data VCalendar = VCalendar
    { vcProdId     :: ProdId
    , vcVersion    :: Version
    , vcCalScale   :: CalScale
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
                             pack (V.showVersion version) <> "//EN") def)
                    (MaxVersion (V.Version [2,0] []) def)
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
                            , vcCalScale   = vcCalScale a
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

-- | Time zone subcomponent, 3.6.5.
--
-- Referred to as either Standard or Daylight.
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
