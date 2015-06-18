{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.ICalendar.Parser.Components where

import           Control.Applicative
import           Control.Arrow                    ((&&&))
import           Control.Monad.Except             hiding (mapM)
import           Control.Monad.RWS                (MonadState (get), tell)
import qualified Data.CaseInsensitive             as CI
import qualified Data.Foldable                    as F
import           Data.List                        (partition)
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Set                         (Set)
import qualified Data.Set                         as S

import           Text.ICalendar.Parser.Common
import           Text.ICalendar.Parser.Properties
import           Text.ICalendar.Types

-- | Parse a VCALENDAR component. 3.4
parseVCalendar :: Content -> ContentParser VCalendar
parseVCalendar c@(Component _ "VCALENDAR" _) = down c $ do
    vcProdId <- reqLine1 "PRODID" (parseSimple ProdId)
    vcVersion <- reqLine1 "VERSION" parseVersion
    vcScale <- optLine1 "CALSCALE" (parseSimpleI Scale)
    vcMethod <- optLine1 "METHOD" (parseSimpleI ((Just .) . Method))
    vcTimeZones <- f (tzidValue . vtzId) =<< optCompN "VTIMEZONE" parseVTimeZone
    vcEvents <- f (uidValue . veUID &&& recur . veRecurId)
                    =<< optCompN "VEVENT" (parseVEvent vcMethod)
    vcTodos <- f (uidValue . vtUID &&& recur . vtRecurId)
                    =<< optCompN "VTODO" parseVTodo
    vcJournals <- f (uidValue . vjUID &&& recur . vjRecurId)
                    =<< optCompN "VJOURNAL" parseVJournal
    vcFreeBusys <- f (uidValue . vfbUID) =<< optCompN "VFREEBUSY" parseVFreeBusy
    vcOtherComps <- otherComponents
    vcOther <- otherProperties
    return VCalendar {..}
  where recur :: Maybe RecurrenceId -> Maybe (Either Date DateTime)
        recur Nothing = Nothing
        recur (Just (RecurrenceIdDate x _ _)) = Just (Left x)
        recur (Just (RecurrenceIdDateTime x _ _)) = Just (Right x)
        f :: Ord b => (a -> b) -> Set a -> ContentParser (M.Map b a)
        f g = F.foldlM h M.empty
          where h m e = let k = g e
                         in if k `M.member` m
                               then throwError "Duplicate UID/RecurId/TZID."
                               else return $ M.insert k e m
parseVCalendar _ = throwError "parseVCalendar: Content given not a VCALENDAR\
                              \ component."

-- | Parse a VEVENT component. 3.6.1
parseVEvent :: Maybe Method -> Content -> ContentParser VEvent
parseVEvent mmethod (Component _ "VEVENT" _) = do
    veDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    veUID <- reqLine1 "UID" $ parseSimple UID
    veDTStart <- optLine1 "DTSTART" $
                  Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    when (isNothing mmethod && isNothing veDTStart) $
        throwError "A VEVENT in a VCALENDAR without a METHOD requires a \
                   \DTSTART property."
    veClass <- optLine1 "CLASS" parseClass
    veCreated <- optLine1 "CREATED" (Just .: parseCreated)
    veDescription <- optLine1 "DESCRIPTION" .
                    parseAltRepLang $ (((Just .) .) .) . Description
    veGeo <- optLine1 "GEO" (Just .: parseGeo)
    veLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    veLocation <- optLine1 "LOCATION" .
        parseAltRepLang $ (((Just .) .) .) . Location
    veOrganizer <- optLine1 "ORGANIZER" (Just .: parseOrganizer)
    vePriority <- optLine1 "PRIORITY" (parseSimpleRead Priority)
    veSeq <- optLine1 "SEQUENCE" (parseSimpleRead Sequence)
    veStatus <- optLine1 "STATUS" (Just .: parseEventStatus)
    veSummary <- optLine1 "SUMMARY" .
                    parseAltRepLang $ (((Just .) .) .) . Summary
    veTransp <- optLine1 "TRANSP" parseTransp
    veUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    veRecurId <- optLine1 "RECURRENCE-ID"
                    $ Just .: parseRecurId veDTStart
    veRRule <- optLineN "RRULE" $ parseRRule veDTStart
    when (S.size veRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    veDTEndDuration <- parseXDurationOpt "DTEND" DTEndDateTime DTEndDate
                                         veDTStart
    veAttach <- optLineN "ATTACH" parseAttachment
    veAttendee <- optLineN "ATTENDEE" parseAttendee
    veCategories <- optLineN "CATEGORIES" parseCategories
    veComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    veContact <- optLineN "CONTACT" $ parseAltRepLang Contact
    veExDate <- optLineN "EXDATE" parseExDate
    veRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    veRelated <- optLineN "RELATED-TO" parseRelatedTo
    veResources <- optLineN "RESOURCES" $ parseAltRepLangN Resources
    veRDate <- optLineN "RDATE" parseRDate
    veAlarms <- optCompN "VALARM" parseVAlarm
    veOther <- otherProperties
    return VEvent {..}
parseVEvent _ x = throwError $ "parseVEvent: " ++ show x

-- | Parse a VTODO component.
parseVTodo :: Content -> ContentParser VTodo
parseVTodo (Component _ "VTODO" _) = do
    vtDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    vtUID <- reqLine1 "UID" $ parseSimple UID
    vtClass <- optLine1 "CLASS" parseClass
    vtCompleted <- optLine1 "COMPLETED" . parseSimpleDateTime $
                (Just .) . Completed
    vtCreated <- optLine1 "CREATED" (Just .: parseCreated)
    vtDTStart <- optLine1 "DTSTART" $
                Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    vtDescription <- optLine1 "DESCRIPTION" .
                parseAltRepLang $ (((Just .) .) .) . Description
    vtGeo <- optLine1 "GEO" (Just .: parseGeo)
    vtLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    vtLocation <- optLine1 "LOCATION" .
                parseAltRepLang $ (((Just .) .) .) . Location
    vtOrganizer <- optLine1 "ORGANIZER" (Just .: parseOrganizer)
    vtPercent <- optLine1 "PERCENT-COMPLETE" $ Just .: parseSimpleRead
                                                             PercentComplete
    vtPriority <- optLine1 "PRIORITY" $ parseSimpleRead Priority
    vtRecurId <- optLine1 "RECURRENCE-ID" (Just .: parseRecurId vtDTStart)
    vtSeq <- optLine1 "SEQUENCE" $ parseSimpleRead Sequence
    vtStatus <- optLine1 "STATUS" (Just .: parseTodoStatus)
    vtSummary <- optLine1 "SUMMARY" .
                parseAltRepLang $ (((Just .) .) .) . Summary
    vtUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    vtRRule <- optLineN "RRULE" $ parseRRule vtDTStart
    when (S.size vtRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    vtDueDuration <- parseXDurationOpt "DUE" DueDateTime DueDate vtDTStart

    vtAttach <- optLineN "ATTACH" parseAttachment
    vtAttendee <- optLineN "ATTENDEE" parseAttendee
    vtCategories <- optLineN "CATEGORIES" parseCategories
    vtComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    vtContact <- optLineN "CONTACT" $ parseAltRepLang Contact
    vtExDate <- optLineN "EXDATE" parseExDate
    vtRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    vtRelated <- optLineN "RELATED-TO" parseRelatedTo
    vtResources <- optLineN "RESOURCES" $ parseAltRepLangN Resources
    vtRDate <- optLineN "RDATE" parseRDate
    vtAlarms <- optCompN "VALARM" parseVAlarm
    vtOther <- otherProperties
    return VTodo {..}
parseVTodo x = throwError $ "parseVTodo: " ++ show x

-- | Parse a VTIMEZONE component. 3.6.5
parseVTimeZone :: Content -> ContentParser VTimeZone
parseVTimeZone (Component _ "VTIMEZONE" _) = do
    vtzId <- reqLine1 "TZID" parseTZID
    vtzLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    vtzUrl <- optLine1 "TZURL" (Just .: parseSimpleURI TZUrl)
    vtzStandardC <- optCompN "STANDARD" parseTZProp
    vtzDaylightC <- optCompN "DAYLIGHT" parseTZProp
    when (S.size vtzStandardC + S.size vtzDaylightC < 1) .
        throwError $ "VTIMEZONE must include at least one of the STANDARD or \
                     \DAYLIGHT components."
    vtzOther <- otherProperties
    return VTimeZone {..}
parseVTimeZone x = throwError $ "parseVTimeZone: " ++ show x

-- | Parse a STANDARD or DAYLIGHT component, tzprop. 3.6.5
parseTZProp :: Content -> ContentParser TZProp
parseTZProp (Component _ n _) | n `elem` ["STANDARD", "DAYLIGHT"] = do
    tzpDTStart <- reqLine1 "DTSTART" $
                    parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    tzpTZOffsetTo <- reqLine1 "TZOFFSETTO" parseUTCOffset
    tzpTZOffsetFrom <- reqLine1 "TZOFFSETFROM" parseUTCOffset
    tzpRRule <- optLineN "RRULE" (parseRRule $ Just tzpDTStart)
    when (S.size tzpRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    tzpComment <- optLineN "COMMENT" (parseAltRepLang Comment)
    tzpRDate <- optLineN "RDATE" parseRDate
    tzpTZName <- optLineN "TZNAME" parseTZName
    tzpOther <- otherProperties
    return TZProp {..}
parseTZProp x = throwError $ "parseTZProp: " ++ show x

-- | Parse a VALARM component. 3.6.6
parseVAlarm :: Content -> ContentParser VAlarm
parseVAlarm (Component _ "VALARM" _) = do
    (ao, a') <- reqLine1 "ACTION" (\(ContentLine _ _ o bs) -> return (o, bs))
    a <- valueOnlyOne =<< parseText a'
    vaTrigger <- reqLine1 "TRIGGER" parseTrigger
    let vaActionOther = toO ao
    case CI.mk a of
         "AUDIO"   -> do
            (vaRepeat, vaDuration) <- repAndDur
            vaAudioAttach <- optLine1 "ATTACH" $ Just .: parseAttachment
            vaOther <- otherProperties
            return VAlarmAudio {..}
         "DISPLAY" -> do
             (vaRepeat, vaDuration) <- repAndDur
             vaDescription <- reqLine1 "DESCRIPTION" $
                 parseAltRepLang Description
             vaOther <- otherProperties
             return VAlarmDisplay {..}
         "EMAIL"   -> do
             (vaRepeat, vaDuration) <- repAndDur
             vaDescription <- reqLine1 "DESCRIPTION" $
                parseAltRepLang Description
             vaSummary <- reqLine1 "SUMMARY" $ parseAltRepLang Summary
             vaAttendee <- reqLineN "ATTENDEE" parseAttendee
             vaMailAttach <- optLineN "ATTACH" parseAttachment
             vaOther <- otherProperties
             return VAlarmEmail {..}
         vaAction  -> do vaOther <- otherProperties
                         return VAlarmX {..}
  where repAndDur = do
             rep <- optLine1 "REPEAT" $ parseSimpleRead Repeat
             dur <- optLine1 "DURATION" $ Just .: parseDurationProp Nothing
             -- Liberal interpretation:
             when (repeatValue rep > 0 && isNothing dur) .
                 throwError $ "parseVAlarm: when REPEAT > 0, DURATION must \
                              \ be specified."
             return (rep, dur)
parseVAlarm x = throwError $ "parseVAlarm: " ++ show x

parseVJournal :: Content -> ContentParser VJournal
parseVJournal (Component _ "VJOURNAL" _) = do
    vjDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    vjUID <- reqLine1 "UID" $ parseSimple UID
    vjClass <- optLine1 "CLASS" parseClass
    vjCreated <- optLine1 "CREATED" (Just .: parseCreated)
    vjDTStart <- optLine1 "DTSTART" $
                Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    vjDescription <- optLineN "DESCRIPTION" $ parseAltRepLang Description
    vjLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    vjOrganizer <- optLine1 "ORGANIZER" (Just .: parseOrganizer)
    vjRecurId <- optLine1 "RECURRENCE-ID" (Just .: parseRecurId vjDTStart)
    vjSeq <- optLine1 "SEQUENCE" $ parseSimpleRead Sequence
    vjStatus <- optLine1 "STATUS" (Just .: parseJournalStatus)
    vjSummary <- optLine1 "SUMMARY" .
                parseAltRepLang $ (((Just .) .) .) . Summary
    vjUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    vjRRule <- optLineN "RRULE" $ parseRRule vjDTStart
    when (S.size vjRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    vjAttach <- optLineN "ATTACH" parseAttachment
    vjAttendee <- optLineN "ATTENDEE" parseAttendee
    vjCategories <- optLineN "CATEGORIES" parseCategories
    vjComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    vjContact <- optLineN "CONTACT" $ parseAltRepLang Contact
    vjExDate <- optLineN "EXDATE" parseExDate
    vjRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    vjRelated <- optLineN "RELATED-TO" parseRelatedTo
    vjRDate <- optLineN "RDATE" parseRDate
    vjOther <- otherProperties
    return VJournal {..}
parseVJournal x = throwError $ "parseVJournal: " ++ show x

parseVFreeBusy :: Content -> ContentParser VFreeBusy
parseVFreeBusy (Component _ "VFreeBusy" _) = do
    vfbDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    vfbUID <- reqLine1 "UID" $ parseSimple UID
    vfbContact <- optLine1 "CONTACT" $ Just .: parseAltRepLang Contact
    vfbDTStart <- optLine1 "DTSTART" $
                  Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    vfbDTEnd <- optLine1 "DTEND" $ Just .: parseSimpleDateOrDateTime
                                                DTEndDateTime DTEndDate
    vfbOrganizer <- optLine1 "ORGANIZER" $ Just .: parseOrganizer
    vfbAttendee <- optLineN "ATTENDEE" parseAttendee
    vfbComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    vfbRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    vfbUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    vfbFreeBusy <- optLineN "FREEBUSY" parseFreeBusy
    vfbOther <- otherProperties
    return VFreeBusy {..}
parseVFreeBusy x = throwError $ "parseVFreeBusy: " ++ show x

otherComponents :: ContentParser (Set VOther)
otherComponents = optN parseVOther . partition isComponent =<< snd <$> get

parseVOther :: Content -> ContentParser VOther
parseVOther (Component _ voName _) = do
    voProps <- otherProperties
    return VOther {..}
parseVOther x = throwError $ "parseVOther: "++ show x

