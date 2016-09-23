module Text.ICalendar.Recurrence
  (
    recurrenceList
  , vEventList
  , vJournalList
  , vTodoList
  , vCalendarTZIDToOffsets
  , vTimeZoneRecurrence
  , RItem(..)
  ) where

import qualified Data.Heap                          as H
import qualified Data.List                          as L
import qualified Data.Map                           as M
import qualified Data.Set                           as S
import           Data.Text.Lazy                     (unpack)

import           Control.Monad                      (join)
import           Data.Default                       (def)
import           Data.Maybe

import           Data.Time                          (TimeZone (..),
                                                     localTimeToUTC, utc)

import           Text.ICalendar.Recurrence.ByRules
import           Text.ICalendar.Recurrence.DateTime (vUTCTimes)
import           Text.ICalendar.Recurrence.Types
import           Text.ICalendar.Types

-- recurrenceList function for VEvent
-- Takes the user's TimeZoneOffset (LocalTime -> Maybe TimeZone)
-- Generally the user should either grab a timezone definition from
-- the vcalendar e.g. (vCalendarTZIDToOffsets vCalendar userTimeZoneIdentifier)
-- use a fixed one e.g. const (Just utc)
vEventList :: TimeToOffset -> VCalendar -> [RItem VEvent]
vEventList = vList vcEvents

-- recurrenceList function for VJournal
-- Takes the user's TimeZoneOffset (LocalTime -> Maybe TimeZone)
-- Generally the user should either grab a timezone definition from
-- the vcalendar e.g. (vCalendarTZIDToOffsets vCalendar userTimeZoneIdentifier)
-- use a fixed one e.g. const (Just utc)
vJournalList :: TimeToOffset -> VCalendar -> [RItem VJournal]
vJournalList = vList vcJournals

-- recurrenceList function for VTodo
-- Takes the user's TimeZoneOffset (LocalTime -> Maybe TimeZone)
-- Generally the user should either grab a timezone definition from
-- the vcalendar e.g. (vCalendarTZIDToOffsets vCalendar userTimeZoneIdentifier)
-- use a fixed one e.g. const (Just utc)
vTodoList :: TimeToOffset -> VCalendar -> [RItem VTodo]
vTodoList = vList vcTodos

vList :: VRecurrence r => (VCalendar -> M.Map a r) -> TimeToOffset -> VCalendar -> [RItem r]
vList f ftz vcal = recurrenceList ftz (vCalendarTZIDToOffsets vcal) (M.elems $ f vcal)

recurrenceList' :: HasRRule r => TimeToOffset -> TZIDToOffset -> [r] -> [RItem r]
recurrenceList' ftz ztz rs = fromHeapList $ H.fromList (rRuleRecurrence ftz ztz <$> rs)

-- The recurrence expansion function. Takes a function to compute the user's timezone
-- A function to compute an arbitrary timezone in the calendar (see vCalendarTZIDToOffsets)
-- and a list of VRecurrence
recurrenceList :: VRecurrence r => TimeToOffset -> TZIDToOffset -> [r] -> [RItem r]
recurrenceList ftz ztz rs = filter ex $ recurrenceList' ftz ztz rs
  where
    ex (RItem _ _ r) = isJust (vRecurId r) || not (S.member (vInstanceID r) exSet)
    exSet = S.fromList $ vRUID <$> filter (isJust . vRecurId) rs

fromHeapList :: Ord r => H.MinHeap [r] -> [r]
fromHeapList  heap = case H.view heap of
  Just (i:is, newHeap) -> i : fromHeapList (case is of [] -> newHeap
                                                       xs -> H.insert xs newHeap)
  Just ([], newHeap) -> fromHeapList newHeap
  Nothing -> []


-- Takes a VCalendar and returns a function which given a timezone ID and a time returns a timezone offset.
vCalendarTZIDToOffsets :: VCalendar -> TZIDToOffset -- Text -> LocalTime -> Maybe TimeZone
vCalendarTZIDToOffsets vcal tz time = join $ flip vTimeZoneToTimeToOffset time <$> M.lookup tz (vcTimeZones vcal)

vTimeZoneToTimeToOffset :: VTimeZone -> TimeToOffset -- LocalTime -> Maybe TimeZone
vTimeZoneToTimeToOffset vtz = let
    getTZ xs lt = tzPropToTimeZone vtz . rItem <$> L.find (cond lt) xs
    cond lt rtz = case rIStartDateUtc rtz of
      Just t  -> t >= localTimeToUTC utc lt
      Nothing -> False
  in getTZ (vTimeZoneRecurrence vtz)

vTimeZoneRecurrence :: VTimeZone -> [RItem TZProp]
vTimeZoneRecurrence vtz = recurrenceList' fErr zErr $ S.toList (vtzStandardC vtz) ++ S.toList (vtzDaylightC vtz)
  where
    fErr = const (Just utc)
    zErr = error "TimeZone recurrences shouldn't use a Zoned Date"

tzPropToTimeZone :: VTimeZone -> TZProp -> TimeZone
tzPropToTimeZone vtz prop = TimeZone (utcOffsetValue (tzpTZOffsetTo prop) `div` 60) (error "Timezone Summer Only undefined") (unpack (tzidValue (vtzId vtz)))
-- TODO figuring out a summer only from the TZProp might be difficult without reworking the data structure undefined for now

rRuleRecurrence :: HasRRule r => TimeToOffset -> TZIDToOffset -> r -> [RItem r]
rRuleRecurrence ftz ztz hr = filters (fromHeapList (H.fromList (initialList hr)))
  where
    filters = filterDuplicateSorted  . filter (isException hr)
    toItem ri = RItem start end ri
      where (start, end) = vUTCTimes ftz ztz ri
    initialList :: HasRRule r => r -> [[RItem r]]
    initialList ri =  if hasRecurrence ri
      then rDateRItemLL ri ++ rRuleRItemLL ri
      else [[toItem ri]]
    rDateRItemLL :: HasRRule r => r -> [[RItem r]]
    rDateRItemLL ri = rItemRDate <$> join (rDateToList <$> S.toList (vRDate ri))
      where rItemRDate prop = [toItem (vUpdateBoth ri prop)]
    rRuleRItemLL :: HasRRule r => r -> [[RItem r]]
    rRuleRItemLL ri = rRuleRItemL <$> S.toList (vRRule ri)
      where rRuleRItemL rrule = toItem <$> rRuleToRRuleFunc rrule ri

isException :: HasRRule r => r -> RItem r -> Bool
isException r ri = case  dtStartValue <$> vDTStart (rItem ri) of
    Just (VDate d)     -> not (S.member d exDateSet)
    Just (VDateTime d) -> not (S.member d exDateTimeSet)
    Nothing            -> False
  where (exDateSet, exDateTimeSet) = foldr foldf def (vExDate r)
        foldf (ExDates a _) (b, c)     = (S.union a b, c)
        foldf (ExDateTimes a _) (b, c) = (b, S.union a c)

rDateToList :: RDate -> [(DTStart, Maybe (Either DTEnd DurationProp))]
rDateToList (RDateDates ds o) = (\x -> (DTStart (VDate x) o, Nothing)) <$> S.elems ds
rDateToList (RDateDateTimes ds o) = (\x -> (DTStart (VDateTime x) o, Nothing)) <$> S.elems ds
rDateToList (RDatePeriods ds o) = toDates <$> S.elems ds
  where
    toDates (PeriodDates x y) = (DTStart (VDateTime x) o, Just (Left (DTEnd (VDateTime y) o)))
    toDates (PeriodDuration x y) = (DTStart (VDateTime x) o, Just (Right (DurationProp y o)))
