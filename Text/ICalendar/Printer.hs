{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Text.ICalendar.Printer
    ( EncodingFunctions(..)
    , printICalendar
    ) where

import           Control.Applicative
import           Control.Arrow                ((&&&))
import           Control.Monad                hiding (forM_, mapM_)
import           Control.Monad.RWS            (MonadState (get, put),
                                               MonadWriter (tell), RWS, asks,
                                               modify, runRWS)
import           Data.ByteString.Lazy         (ByteString)
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as Bu
import qualified Data.ByteString.Lazy.Char8   as BS
import qualified Data.CaseInsensitive         as CI
import           Data.Char                    (ord, toUpper)
import           Data.Default
import           Data.Foldable                (forM_, mapM_)
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as S
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as T
import           Data.Time                    (FormatTime ())
import qualified Data.Time                    as Time
import qualified Data.Version                 as Ver
import qualified Network.URI                  as URI
import           Prelude                      hiding (mapM_)

#if MIN_VERSION_time(1,5,0)
import           Data.Time                    (defaultTimeLocale)
#else
import           System.Locale                (defaultTimeLocale)
#endif

import           Text.Printf                  (printf)

import           Codec.MIME.Type              (MIMEType, showMIMEType)
import qualified Data.ByteString.Base64.Lazy  as B64

import           Text.ICalendar.Types


-- | Functions for encoding into bytestring builders.
data EncodingFunctions = EncodingFunctions
    { efChar2Bu  :: Char -> Builder
    , efChar2Len :: Char -> Int -- ^ How many octets the character is encoded.
    }

utf8Len :: Char -> Int
utf8Len c | o < 0x80  = 1
          | o < 0x800  = 2
          | o < 0x10000 = 3
          | o < 0x200000 = 4
          | o < 0x4000000 = 5
          | otherwise      = 6
  where o = ord c

data Quoting = NeedQuotes | Optional | NoQuotes
               deriving (Eq, Ord, Show)

-- | UTF8.
instance Default EncodingFunctions where
    def = EncodingFunctions Bu.charUtf8
                            utf8Len

type ContentPrinter = RWS EncodingFunctions Builder Int

-- | Print a VCalendar object to a ByteString.
printICalendar :: EncodingFunctions -> VCalendar -> ByteString
printICalendar r v = (\(_, _, x) -> Bu.toLazyByteString x) $
                     runRWS (printVCalendar v) r 0

-- {{{ Component printers

printVCalendar :: VCalendar -> ContentPrinter ()
printVCalendar VCalendar {..} = do
    line "BEGIN:VCALENDAR"
    ln $ do prop "VERSION" $ versionOther vcVersion -- Should be first for
            printValue vcVersion                    -- compatibility.
    ln $ do prop "PRODID" $ prodIdOther vcProdId
            text $ prodIdValue vcProdId
    ln $ do prop "CALSCALE" $ scaleOther vcScale
            text . CI.original $ scaleValue vcScale
    forM_ vcMethod $ \meth -> do
        prop "METHOD" $ methodOther meth
        ln . text . CI.original $ methodValue meth
    mapM_ printProperty vcOther
    mapM_ printVTimeZone vcTimeZones
    mapM_ printVEvent vcEvents
    mapM_ printVTodo vcTodos
    mapM_ printVJournal vcJournals
    mapM_ printVFreeBusy vcFreeBusys
    mapM_ printVOther vcOtherComps
    line "END:VCALENDAR"

printVTimeZone :: VTimeZone -> ContentPrinter ()
printVTimeZone VTimeZone {..} = do
    line "BEGIN:VTIMEZONE"
    ln $ do prop "TZID" $ tzidOther vtzId
            text $ tzidValue vtzId
    printProperty vtzLastMod
    forM_ vtzUrl $ \url -> do
        prop "TZURL" $ tzUrlOther url
        ln . printShow $ tzUrlValue url
    mapM_ (printTZProp "STANDARD") vtzStandardC
    mapM_ (printTZProp "DAYLIGHT") vtzDaylightC
    mapM_ printProperty vtzOther
    line "END:VTIMEZONE"

printTZProp :: ByteString -> TZProp -> ContentPrinter ()
printTZProp name TZProp {..} = do
    line $ "BEGIN:" <> name
    printProperty tzpDTStart
    ln $ do prop "TZOFFSETTO" $ utcOffsetOther tzpTZOffsetTo
            printUTCOffset $ utcOffsetValue tzpTZOffsetTo
    ln $ do prop "TZOFFSETFROM" $ utcOffsetOther tzpTZOffsetTo
            printUTCOffset $ utcOffsetValue tzpTZOffsetFrom
    printProperty tzpRRule
    printProperty tzpComment
    printProperty tzpRDate
    forM_ tzpTZName $ \TZName {..} -> ln $ do
        prop "TZNAME" $ toParam tzNameLanguage <> toParam tzNameOther
        text tzNameValue
    mapM_ printProperty tzpOther
    line $ "END:" <> name

printVEvent :: VEvent -> ContentPrinter ()
printVEvent VEvent {..} = do
    line "BEGIN:VEVENT"
    printProperty veDTStamp
    printProperty veUID
    printProperty veDTStart
    printProperty veClass
    printProperty veCreated
    printProperty veDescription
    printProperty veGeo
    printProperty veLastMod
    printProperty veLocation
    printProperty veOrganizer
    printProperty vePriority
    printProperty veSeq
    printProperty veStatus
    printProperty veSummary
    printProperty veTransp
    printProperty veUrl
    printProperty veRecurId
    printProperty veRRule
    printProperty veDTEndDuration
    printProperty veAttach
    printProperty veAttendee
    printProperty veCategories
    printProperty veComment
    printProperty veContact
    printProperty veExDate
    printProperty veRStatus
    printProperty veRelated
    printProperty veResources
    printProperty veRDate
    forM_ veAlarms printVAlarm
    printProperty veOther
    line "END:VEVENT"

printVTodo :: VTodo -> ContentPrinter ()
printVTodo VTodo {..} = do
    line "BEGIN:VTODO"
    printProperty vtDTStamp
    printProperty vtUID
    printProperty vtClass
    printProperty vtCompleted
    printProperty vtCreated
    printProperty vtDescription
    printProperty vtDTStart
    printProperty vtGeo
    printProperty vtLastMod
    printProperty vtLocation
    printProperty vtOrganizer
    printProperty vtPercent
    printProperty vtPriority
    printProperty vtSeq
    printProperty vtRecurId
    printProperty vtStatus
    printProperty vtSummary
    printProperty vtUrl
    printProperty vtRRule
    printProperty vtDueDuration
    printProperty vtAttach
    printProperty vtAttendee
    printProperty vtCategories
    printProperty vtComment
    printProperty vtContact
    printProperty vtExDate
    printProperty vtRStatus
    printProperty vtRelated
    printProperty vtResources
    printProperty vtRDate
    forM_ vtAlarms printVAlarm
    printProperty vtOther
    line "END:VTODO"

printVJournal :: VJournal -> ContentPrinter ()
printVJournal VJournal {..} = do
    line "BEGIN:VJOURNAL"
    printProperty vjDTStamp
    printProperty vjUID
    printProperty vjClass
    printProperty vjCreated
    printProperty vjDescription
    printProperty vjDTStart
    printProperty vjLastMod
    printProperty vjOrganizer
    printProperty vjSeq
    printProperty vjRecurId
    printProperty vjStatus
    printProperty vjSummary
    printProperty vjUrl
    printProperty vjRRule
    printProperty vjAttach
    printProperty vjAttendee
    printProperty vjCategories
    printProperty vjComment
    printProperty vjContact
    printProperty vjExDate
    printProperty vjRStatus
    printProperty vjRelated
    printProperty vjRDate
    printProperty vjOther
    line "END:VJOURNAL"

printVFreeBusy :: VFreeBusy -> ContentPrinter ()
printVFreeBusy VFreeBusy {..} = do
    line "BEGIN:VFREEBUSY"
    printProperty vfbDTStamp
    printProperty vfbUID
    printProperty vfbContact
    printProperty vfbDTStart
    printProperty vfbDTEnd
    printProperty vfbOrganizer
    printProperty vfbUrl
    printProperty vfbAttendee
    printProperty vfbComment
    printProperty vfbFreeBusy
    printProperty vfbRStatus
    printProperty vfbOther
    line "END:VFREEBUSY"

printVOther :: VOther -> ContentPrinter ()
printVOther VOther {..} = do
    ln . out $ "BEGIN:V" <> CI.original voName
    mapM_ printProperty voProps
    ln . out $ "END:V" <> CI.original voName

printVAlarm :: VAlarm -> ContentPrinter ()
printVAlarm va = do
    line "BEGIN:VALARM"
    prop "ACTION" $ vaActionOther va
    case va of
         VAlarmAudio {..} -> do
             ln $ bytestring "AUDIO"
             printProperty vaTrigger
             repAndDur
             printProperty vaAudioAttach
             printProperty vaOther
         VAlarmDisplay {..} -> do
             ln $ bytestring "DISPLAY"
             printProperty vaTrigger
             printProperty vaDescription
             repAndDur
             printProperty vaOther
         VAlarmEmail {..} -> do
             ln $ bytestring "EMAIL"
             printProperty vaTrigger
             printProperty vaDescription
             printProperty vaSummary
             printProperty vaAttendee
             repAndDur
             printProperty vaMailAttach
             printProperty vaOther
         VAlarmX {..} -> do
             ln . out $ CI.original vaAction
             printProperty vaTrigger
             printProperty vaOther
    line "END:VALARM"
  where repAndDur = unless (vaRepeat va == def) $ do
            printProperty $ vaRepeat va
            unless (repeatValue (vaRepeat va) == 0) $
                forM_ (vaDuration va) printProperty

-- }}}
-- {{{ Property printers.

class IsProperty a where
    printProperty :: a -> ContentPrinter ()

instance IsProperty a => IsProperty (Set a) where
    printProperty = mapM_ printProperty

instance IsProperty a => IsProperty (Maybe a) where
    printProperty (Just x) = printProperty x
    printProperty _ = return ()

instance (IsProperty a, IsProperty b) => IsProperty (Either a b) where
    printProperty (Left x) = printProperty x
    printProperty (Right x) = printProperty x

instance IsProperty FreeBusy where
    printProperty FreeBusy {..} = ln $ do
        prop "FREEBUSY" $ toParam freeBusyOther <> toParam freeBusyType
        printN printValue $ S.toList freeBusyPeriods

instance IsProperty PercentComplete where
    printProperty PercentComplete {..} = ln $ do
        prop "PERCENT-COMPLETE" percentCompleteOther
        printShow percentCompleteValue

instance IsProperty Completed where
    printProperty Completed {..} = ln $ do prop "COMPLETED" completedOther
                                           printValue completedValue

instance IsProperty DurationProp where
    printProperty DurationProp {..} = ln $ do prop "DURATION" durationOther
                                              printValue durationValue

instance IsProperty Repeat where
    printProperty Repeat {..} = ln $ do prop "REPEAT" repeatOther
                                        printShow repeatValue

instance IsProperty DTEnd where
    printProperty dtend = ln $ prop "DTEND" dtend >> printValue dtend

instance IsProperty Due where
    printProperty due = ln $ prop "DUE" due >> printValue due

instance IsProperty DTStamp where
    printProperty x = ln $ prop "DTSTAMP" x >> printValue x

instance IsProperty UID where
    printProperty UID {..} = ln $ prop "UID" uidOther >> text uidValue

instance IsProperty DTStart where
    printProperty x = ln $ prop "DTSTART" x >> printValue x

instance IsProperty Class where
    printProperty c@Class {..} | c == def = return ()
                               | otherwise = ln $ do prop "CLASS" classOther
                                                     printValue classValue

instance IsProperty Created where
    printProperty Created {..} = ln $ do
        prop "CREATED" createdOther
        printUTCTime createdValue

instance IsProperty Description  where
    printProperty Description {..} = ln $ do
        prop "DESCRIPTION" $ toParam (AltRep <$> descriptionAltRep) <>
                             toParam descriptionLanguage <>
                             toParam descriptionOther
        text descriptionValue

instance IsProperty Geo where
    printProperty Geo {..} = ln $ do
        prop "GEO" geoOther
        out . T.pack $ printf "%.6f;%.6f" geoLat geoLong

instance IsProperty LastModified where
    printProperty LastModified {..} = ln $ do
        prop "LAST-MODIFIED" lastModifiedOther
        printUTCTime lastModifiedValue

instance IsProperty Location where
    printProperty Location {..} = ln $ do
        prop "LOCATION" $ toParam (AltRep <$> locationAltRep) <>
                          toParam locationLanguage <> toParam locationOther
        text locationValue

instance IsProperty Organizer where
    printProperty Organizer {..} = ln $ do
        prop "ORGANIZER" $ toParam (CN <$> organizerCN) <>
                           toParam (Dir <$> organizerDir) <>
                           toParam (SentBy <$> organizerSentBy) <>
                           toParam organizerLanguage <> toParam organizerOther
        printShow organizerValue

instance IsProperty Priority where
    printProperty x | x == def = return ()
                    | otherwise = ln $ do prop "PRIORITY" $ priorityOther x
                                          printShow $ priorityValue x

instance IsProperty Sequence where
    printProperty x | x == def = return ()
                    | otherwise = ln $ do prop "SEQUENCE" $ sequenceOther x
                                          printShow $ sequenceValue x
instance IsProperty EventStatus where
    printProperty s = ln $ do prop "STATUS" $ eventStatusOther s
                              printValue s

instance IsProperty TodoStatus where
    printProperty s = ln $ do prop "STATUS" $ todoStatusOther s
                              printValue s

instance IsProperty JournalStatus where
    printProperty s = ln $ do prop "STATUS" $ journalStatusOther s
                              printValue s

instance IsProperty Summary where
    printProperty Summary {..} = ln $ do
        prop "SUMMARY" $ toParam (AltRep <$> summaryAltRep) <>
                         toParam summaryLanguage <> toParam summaryOther
        text summaryValue

instance IsProperty Transp where
    printProperty x | x == def = return ()
                    | otherwise = ln $ do
                        prop "TRANSP" $ timeTransparencyOther x
                        printValue x

instance IsProperty URL where
    printProperty URL {..} = ln $ prop "URL" urlOther >> printShow urlValue

instance IsProperty RecurrenceId where
    printProperty r = ln $ prop "RECURRENCE-ID" r >> printValue r

instance IsProperty RRule where
    printProperty RRule {..} = ln $ do prop "RRULE" rRuleOther
                                       printValue rRuleValue

instance IsProperty Attach where
    printProperty a = ln $ prop "ATTACH" a >> printValue a

instance IsProperty Attendee where
    printProperty att@Attendee {..} = ln $ do
        prop "ATTENDEE" att
        printValue attendeeValue

instance IsProperty Categories where
    printProperty Categories {..} = ln $ do
        prop "CATEGORIES" $ toParam categoriesOther <>
                            toParam categoriesLanguage
        texts $ S.toList categoriesValues

instance IsProperty Comment where
    printProperty Comment {..} = ln $ do
        prop "COMMENT" $ toParam (AltRep <$> commentAltRep) <>
                         toParam commentLanguage <>
                         toParam commentOther
        text commentValue

instance IsProperty Contact where
    printProperty Contact {..} = ln $ do
        prop "CONTACT" $ toParam (AltRep <$> contactAltRep) <>
                         toParam contactLanguage <>
                         toParam contactOther
        text contactValue

instance IsProperty ExDate where
    printProperty exd = ln $ do
        prop "EXDATE" exd
        case exd of
             ExDates {..} -> printN printValue $ S.toList exDates
             ExDateTimes {..} -> printN printValue $ S.toList exDateTimes

instance IsProperty RequestStatus where
    printProperty RequestStatus {..} = ln $ do
        prop "REQUEST-STATUS" $ toParam requestStatusLanguage <>
                                toParam requestStatusOther
        (\z -> case z of
                    (x:xs) -> do printShow x
                                 sequence_ [putc '.' >> printShow y | y <- xs]
                    [] -> return ()) requestStatusCode
        putc ';'
        text requestStatusDesc
        forM_ requestStatusExt $ \x -> putc ';' >> text x

instance IsProperty RelatedTo where
    printProperty RelatedTo {..} = ln $ do
        prop "RELATED-TO" $ toParam relatedToOther <> toParam relatedToType
        text relatedToValue

instance IsProperty Resources where
    printProperty Resources {..} = ln $ do
        prop "RESOURCES" $ toParam (AltRep <$> resourcesAltRep) <>
                           toParam resourcesLanguage <> toParam resourcesOther
        texts $ S.toList resourcesValue

instance IsProperty RDate where
    printProperty r = ln $ prop "RDATE" r >> printValue r

instance IsProperty OtherProperty where
    printProperty OtherProperty {..} = ln $ do
        out (CI.original otherName)
        mapM_ param $ toParam otherParams
        out ":"
        bytestring otherValue

instance IsProperty Trigger where
    printProperty tr@TriggerDuration {..} = ln $ do
        prop "TRIGGER" tr
        printValue triggerDuration
    printProperty tr@TriggerDateTime {..} = ln $ do
        prop "TRIGGER" tr
        printUTCTime triggerDateTime


-- | Print a generic property.
prop :: ToParam a
     => ByteString
     -> a
     -> ContentPrinter ()
prop b x = do
    put (fromIntegral $ BS.length b)
    tell (Bu.lazyByteString b)
    mapM_ param $ toParam x
    out ":"

-- }}}
-- {{{ Parameter "printers".

class ToParam a where
    toParam :: a -> [(Text, [(Quoting, Text)])]

instance ToParam a => ToParam (Maybe a) where
    toParam Nothing = []
    toParam (Just x) = toParam x

instance ToParam a => ToParam (Set a) where
    toParam s = case S.maxView s of
                     Nothing -> []
                     Just (x, _) -> toParam x

instance ToParam ExDate where
    toParam ExDates {..} = [("VALUE", [(NoQuotes, "DATE")])] <>
                           toParam exDateOther
    toParam ExDateTimes {..} = toParam exDateOther <>
                               toParam (fst <$> S.maxView exDateTimes)

instance ToParam AltRep where
    toParam (AltRep x) = [("ALTREP", [(NeedQuotes, T.pack $ show x)])]

instance ToParam SentBy where
    toParam (SentBy x) = [("SENT-BY", [(NeedQuotes, T.pack $ show x)])]

instance ToParam Dir where
    toParam (Dir x) = [("DIR", [(NeedQuotes, T.pack $ show x)])]

instance ToParam DateTime where
    toParam ZonedDateTime {..} = [("TZID", [(Optional, dateTimeZone)])]
    toParam _ = []

instance ToParam DTEnd where
    toParam DTEndDateTime {..} = toParam dtEndOther <>
                                 toParam dtEndDateTimeValue
    toParam DTEndDate {..}     = [("VALUE", [(NoQuotes, "DATE")])] <>
                                 toParam dtEndOther

instance ToParam Due where
    toParam DueDateTime {..} = toParam dueOther <> toParam dueDateTimeValue
    toParam DueDate {..}     = [("VALUE", [(NoQuotes, "DATE")])] <>
                               toParam dueOther

instance ToParam CN where
    toParam (CN x) = [("CN", [(Optional, x)])]

instance ToParam DTStart where
    toParam DTStartDateTime {..} = toParam dtStartDateTimeValue <>
                                   toParam dtStartOther
    toParam DTStartDate {..} = [("VALUE", [(NoQuotes, "DATE")])] <>
                               toParam dtStartOther

instance ToParam RDate where
    toParam RDateDates {..} = [("VALUE", [(NoQuotes, "DATE")])] <>
                              toParam rDateOther
    toParam RDatePeriods {..} = [("VALUE", [(NoQuotes, "PERIOD")])] <>
                                toParam rDateOther <>
                                toParam (fst <$> S.maxView rDatePeriods)
    toParam RDateDateTimes {..} = toParam rDateDateTimes <> toParam rDateOther

instance ToParam Period where
    toParam (PeriodDates x _) = toParam x
    toParam (PeriodDuration x _) = toParam x

instance ToParam DTStamp where
    toParam DTStamp {..} = toParam dtStampOther

instance ToParam OtherParams where
    toParam (OtherParams l) = fromOP <$> S.toList l
      where fromOP (OtherParam x y) = (CI.original x, (Optional,) <$> y)

instance ToParam Language where
    toParam (Language x) = [("LANGUAGE", [(Optional, CI.original x)])]

instance ToParam TZName where
    toParam TZName {..} = toParam tzNameLanguage <> toParam tzNameOther

instance ToParam x => ToParam [x] where
    toParam = mconcat . map toParam

instance ToParam (Text, [(Quoting, Text)]) where
    toParam = (:[])

instance ToParam RecurrenceId where
    toParam RecurrenceIdDate {..} = [("VALUE", [(NoQuotes, "DATE")])] <>
                                    toParam recurrenceIdRange <>
                                    toParam recurrenceIdOther
    toParam RecurrenceIdDateTime {..} = toParam recurrenceIdDateTime <>
                                        toParam recurrenceIdRange <>
                                        toParam recurrenceIdOther

instance ToParam Range where
    toParam ThisAndFuture = [("RANGE", [(NoQuotes, "THISANDFUTURE")])]
    toParam _ = [] -- ThisAndPrior MUST NOT be generated.

instance ToParam FBType where
    toParam x | x == def    = []
    toParam Free            = [("FBTYPE", [(NoQuotes, "FREE")])]
    toParam Busy            = [("FBTYPE", [(NoQuotes, "BUSY")])]
    toParam BusyUnavailable = [("FBTYPE", [(NoQuotes, "BUSY-UNAVAILABLE")])]
    toParam BusyTentative   = [("FBTYPE", [(NoQuotes, "BUSY-TENTATIVE")])]
    toParam (FBTypeX x)     = [("FBTYPE", [(Optional, CI.original x)])]

instance ToParam MIMEType where
    toParam m = [("FMTTYPE", [(NoQuotes, T.fromStrict $ showMIMEType m)])]

instance ToParam Attach where
    toParam UriAttach {..} = toParam attachFmtType <>
                             toParam attachOther
    toParam BinaryAttach {..} = toParam attachFmtType <>
                                toParam attachOther <>
                                [ ("VALUE", [(NoQuotes, "BINARY")])
                                , ("ENCODING", [(NoQuotes, "BASE64")])]

instance ToParam CUType where
    toParam x | x == def = []
    toParam Individual   = [("CUTYPE", [(NoQuotes, "INDIVIDUAL")])]
    toParam Group        = [("CUTYPE", [(NoQuotes, "GROUP")])]
    toParam Resource     = [("CUTYPE", [(NoQuotes, "RESOURCE")])]
    toParam Room         = [("CUTYPE", [(NoQuotes, "ROOM")])]
    toParam Unknown      = [("CUTYPE", [(NoQuotes, "UNKNOWN")])]
    toParam (CUTypeX x)  = [("CUTYPE", [(Optional, CI.original x)])]

instance ToParam Member where
    toParam (Member x) | S.null x = []
    toParam (Member x) = [( "MEMBER"
                          , (NeedQuotes,) . T.pack . show <$> S.toList x)]

instance ToParam Role where
    toParam x | x == def   = []
    toParam Chair          = [("ROLE", [(NoQuotes, "CHAIR")])]
    toParam ReqParticipant = [("ROLE", [(NoQuotes, "REQ-PARTICIPANT")])]
    toParam OptParticipant = [("ROLE", [(NoQuotes, "OPT-PARTICIPANT")])]
    toParam NonParticipant = [("ROLE", [(NoQuotes, "NON-PARTICIPANT")])]
    toParam (RoleX x)      = [("ROLE", [(Optional, CI.original x)])]

instance ToParam PartStat where
    toParam x | x == def        = []
    toParam PartStatNeedsAction = [("PARTSTAT", [(NoQuotes, "NEEDS-ACTION")])]
    toParam Accepted            = [("PARTSTAT", [(NoQuotes, "ACCEPTED")])]
    toParam Declined            = [("PARTSTAT", [(NoQuotes, "DECLINED")])]
    toParam Tentative           = [("PARTSTAT", [(NoQuotes, "TENTATIVE")])]
    toParam Delegated           = [("PARTSTAT", [(NoQuotes, "DELEGATED")])]
    toParam PartStatCompleted   = [("PARTSTAT", [(NoQuotes, "COMPLETED")])]
    toParam InProcess           = [("PARTSTAT", [(NoQuotes, "IN-PROCESS")])]
    toParam (PartStatX x)       = [("PARTSTAT", [(Optional, CI.original x)])]

instance ToParam RelType where
    toParam x | x == def = []
    toParam Parent       = [("RELTYPE", [(NoQuotes, "PARENT")])]
    toParam Child        = [("RELTYPE", [(NoQuotes, "CHILD")])]
    toParam Sibling      = [("RELTYPE", [(NoQuotes, "SIBLING")])]
    toParam (RelTypeX x) = [("RELTYPE", [(Optional, CI.original x)])]

instance ToParam RSVP where
    toParam x | x == def = []
    toParam (RSVP False) = [("RSVP", [(NoQuotes, "FALSE")])]
    toParam (RSVP True)  = [("RSVP", [(NoQuotes, "TRUE")])]

instance ToParam DelegatedTo where
    toParam (DelegatedTo x) | S.null x  = []
                            | otherwise = [( "DELEGATED-TO"
                                           , (NeedQuotes,) . T.pack . show
                                                 <$> S.toList x)]

instance ToParam DelegatedFrom where
    toParam (DelegatedFrom x) | S.null x = []
                              | otherwise = [( "DELEGATED-FROM"
                                             , (NeedQuotes,) . T.pack . show
                                                   <$> S.toList x)]

instance ToParam Attendee where
    toParam Attendee {..} = toParam attendeeCUType <>
                            toParam (Member attendeeMember) <>
                            toParam attendeeRole <>
                            toParam attendeePartStat <>
                            toParam (RSVP attendeeRSVP) <>
                            toParam (DelegatedTo attendeeDelTo) <>
                            toParam (DelegatedFrom attendeeDelFrom) <>
                            toParam (SentBy <$> attendeeSentBy) <>
                            toParam (CN <$> attendeeCN) <>
                            toParam (Dir <$> attendeeDir) <>
                            toParam attendeeLanguage <>
                            toParam attendeeOther

instance ToParam Related where
    toParam x | x == def = []
    toParam Start        = [("RELATED", [(NoQuotes, "START")])]
    toParam End          = [("RELATED", [(NoQuotes, "END")])]

instance ToParam Trigger where
    toParam TriggerDuration {..} = toParam triggerOther <>
                                   toParam triggerRelated
    toParam TriggerDateTime {..} = toParam triggerOther <>
                                   [("VALUE", [(NoQuotes, "DATE-TIME")])]

-- }}}
-- {{{ Value printers

printUTCOffset :: Int -> ContentPrinter ()
printUTCOffset n = do case signum n of
                           -1 -> putc '-'
                           _  -> putc '+'
                      out . T.pack $ printf "%02d" t
                      out . T.pack $ printf "%02d" m
                      when (s > 0) . out . T.pack $ printf "%02d" s
  where (m', s) = abs n `divMod` 60
        (t, m)  = m' `divMod` 60

printNWeekday :: Either (Int, Weekday) Weekday -> ContentPrinter ()
printNWeekday (Left (n, w)) = printShow n >> printValue w
printNWeekday (Right x) = printValue x

printShow :: Show a => a -> ContentPrinter ()
printShow = out . T.pack . show

printShowN :: Show a => [a] -> ContentPrinter ()
printShowN = printN printShow

printN :: (a -> ContentPrinter ()) -> [a] -> ContentPrinter ()
printN m (x:xs) = m x >> sequence_ [putc ',' >> m x' | x' <- xs]
printN _ _ = return ()

printShowUpper :: Show a => a -> ContentPrinter ()
printShowUpper = out . T.pack . map toUpper . show

printUTCTime :: Time.UTCTime -> ContentPrinter ()
printUTCTime = out . T.pack . formatTime "%C%y%m%dT%H%M%SZ"

class IsValue a where
    printValue :: a -> ContentPrinter ()

instance IsValue ICalVersion where
    printValue MaxICalVersion {..} = out . T.pack $ Ver.showVersion versionMax
    printValue MinMaxICalVersion {..} = do
        out . T.pack $ Ver.showVersion versionMin
        putc ';'
        out . T.pack $ Ver.showVersion versionMax

instance IsValue Recur where
    printValue Recur {..} = do
        out "FREQ="
        printShowUpper recurFreq
        forM_ recurUntilCount $ \x ->
            case x of
                 Left y -> out ";UNTIL=" >> printValue y
                 Right y -> out ";COUNT=" >> printShow y
        when (recurInterval /= 1) $
            out ";INTERVAL=" >> printShow recurInterval
        unless (null recurBySecond) $
            out ";BYSECOND=" >> printShowN recurBySecond
        unless (null recurByMinute) $
            out ";BYMINUTE=" >> printShowN recurByMinute
        unless (null recurByHour) $
            out ";BYHOUR=" >> printShowN recurByHour
        unless (null recurByDay) $
            out ";BYDAY=" >> printN printNWeekday recurByDay
        unless (null recurByMonthDay) $
            out ";BYMONTHDAY=" >> printShowN recurByMonthDay
        unless (null recurByYearDay) $
            out ";BYYEARDAY=" >> printShowN recurByYearDay
        unless (null recurByWeekNo) $
            out ";BYWEEKNO=" >> printShowN recurByWeekNo
        unless (null recurByMonth) $
            out ";BYMONTH=" >> printShowN recurByMonth
        unless (null recurBySetPos) $
            out ";BYSETPOS=" >> printShowN recurBySetPos
        unless (recurWkSt == Monday) $
            out ";WKST=" >> printValue recurWkSt

instance IsValue Transp where
    printValue Opaque {}      = out "OPAQUE"
    printValue Transparent {} = out "TRANSPARENT"

instance IsValue DTEnd where
    printValue DTEndDateTime {..} = printValue dtEndDateTimeValue
    printValue DTEndDate {..}     = printValue dtEndDateValue

instance IsValue Due where
    printValue DueDateTime {..} = printValue dueDateTimeValue
    printValue DueDate {..}     = printValue dueDateValue

instance IsValue EventStatus where
    printValue TentativeEvent {} = out "TENTATIVE"
    printValue ConfirmedEvent {} = out "CONFIRMED"
    printValue CancelledEvent {} = out "CANCELLED"

instance IsValue TodoStatus where
    printValue TodoNeedsAction {} = out "NEEDS-ACTION"
    printValue CompletedTodo {}   = out "COMPLETED"
    printValue InProcessTodo {}   = out "IN-PROCESS"
    printValue CancelledTodo {}   = out "CANCELLED"

instance IsValue JournalStatus where
    printValue DraftJournal {}     = out "DRAFT"
    printValue FinalJournal {}     = out "FINAL"
    printValue CancelledJournal {} = out "CANCELLED"

instance IsValue ClassValue where
    printValue (ClassValueX x) = out $ CI.original x
    printValue x = printShowUpper x

instance IsValue Weekday where
    printValue Sunday    = out "SU"
    printValue Monday    = out "MO"
    printValue Tuesday   = out "TU"
    printValue Wednesday = out "WE"
    printValue Thursday  = out "TH"
    printValue Friday    = out "FR"
    printValue Saturday  = out "SA"

instance IsValue Date where
    printValue Date {..} = out . T.pack $ formatTime "%C%y%m%d" dateValue

instance IsValue DateTime where
    printValue FloatingDateTime {..} =
        out . T.pack $ formatTime "%C%y%m%dT%H%M%S" dateTimeFloating
    printValue UTCDateTime {..} = printUTCTime dateTimeUTC
    printValue ZonedDateTime {..} =
        out . T.pack $ formatTime "%C%y%m%dT%H%M%S" dateTimeFloating

instance IsValue (Either Date DateTime) where
    printValue (Left x) = printValue x
    printValue (Right x) = printValue x

instance IsValue DTStamp where
    printValue DTStamp {..} = printUTCTime dtStampValue

instance IsValue DTStart where
    printValue DTStartDateTime {..} = printValue dtStartDateTimeValue
    printValue DTStartDate {..} = printValue dtStartDateValue

instance IsValue URI.URI where
    printValue = printShow

instance IsValue Duration where
    printValue DurationDate {..} = do
        when (durSign == Negative) $ putc '-'
        putc 'P'
        printShow durDay    >> putc 'D'
        putc 'T'
        printShow durHour   >> putc 'H'
        printShow durMinute >> putc 'M'
        printShow durSecond >> putc 'S'
    printValue DurationTime {..} = do
        when (durSign == Negative) $ putc '-'
        out "PT"
        printShow durHour   >> putc 'H'
        printShow durMinute >> putc 'M'
        printShow durSecond >> putc 'S'
    printValue DurationWeek {..} = do
        when (durSign == Negative) $ putc '-'
        out "P"
        printShow durWeek >> putc 'W'

instance IsValue RecurrenceId where
    printValue RecurrenceIdDate {..}     = printValue recurrenceIdDate
    printValue RecurrenceIdDateTime {..} = printValue recurrenceIdDateTime

instance IsValue Period where
    printValue (PeriodDates f t) = printValue f >> putc '/' >> printValue t
    printValue (PeriodDuration f d) = printValue f >> putc '/' >> printValue d

instance IsValue UTCPeriod where
    printValue (UTCPeriodDates f t) = printUTCTime f >> putc '/' >> printUTCTime t
    printValue (UTCPeriodDuration f d) = printUTCTime f >> putc '/' >> printValue d

instance IsValue RDate where
    printValue RDateDates {..}     = printN printValue $ S.toList rDateDates
    printValue RDateDateTimes {..} = printN printValue $ S.toList rDateDateTimes
    printValue RDatePeriods {..}   = printN printValue $ S.toList rDatePeriods

instance IsValue Attach where
    printValue UriAttach {..} = printShow attachUri
    printValue BinaryAttach {..} = bytestring $ B64.encode attachContent

-- }}}
-- {{{ Lib

ln :: ContentPrinter () -> ContentPrinter ()
ln x = x >> newline

param :: (Text, [(Quoting, Text)]) -> ContentPrinter ()
param (n, xs) = putc ';' >> out n >> putc '=' >> paramVals xs

paramVals :: [(Quoting, Text)] -> ContentPrinter ()
paramVals (x:xs) = paramVal x >> sequence_ [putc ',' >> paramVal x' | x' <- xs]
paramVals _ = return ()

paramVal :: (Quoting, Text) -> ContentPrinter ()
paramVal (NeedQuotes, t) = putc '"' >> out t >> putc '"'
paramVal (NoQuotes, t) = out t
paramVal (_, t) = paramVal (NeedQuotes, t)

texts :: [Text] -> ContentPrinter ()
texts (x:xs) = text x >> sequence_ [putc ',' >> text x' | x' <- xs]
texts _ = return ()

text :: Text -> ContentPrinter ()
text t = case T.uncons t of
              Just (';', r)  -> out "\\;"  >> text r
              Just ('\n', r) -> out "\\n"  >> text r
              Just (',', r)  -> out "\\,"  >> text r
              Just ('\\', r) -> out "\\\\" >> text r
              Just (c, r)    -> putc c     >> text r
              Nothing        -> return ()

bytestring :: ByteString -> ContentPrinter ()
bytestring = BS.foldl' (\m c -> m >> putc8 c) (return ())

out :: Text -> ContentPrinter ()
out t = case T.uncons t of
             Just (c, r) -> putc c >> out r
             Nothing -> return ()

putc :: Char -> ContentPrinter ()
putc c = do x <- get
            (b, clen) <- asks (efChar2Bu &&& efChar2Len)
            let cl = clen c
            when (x + cl > 75) foldLine
            tell $ b c
            modify (+ cl)

putc8 :: Char -> ContentPrinter ()
putc8 c = do x <- get
             when (x >= 75) foldLine
             tell $ Bu.char8 c
             modify (+ 1)

foldLine :: ContentPrinter ()
foldLine = tell (Bu.byteString "\r\n ") >> put 1

newline :: ContentPrinter ()
newline = tell (Bu.byteString "\r\n") >> put 0

-- | Output a whole line. Must be less than 75 bytes.
line :: ByteString -> ContentPrinter ()
line b = tell (Bu.lazyByteString b) >> newline

formatTime :: FormatTime t => String -> t -> String
formatTime = Time.formatTime defaultTimeLocale

-- }}}
