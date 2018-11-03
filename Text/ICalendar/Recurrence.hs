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
vEventList :: TimeToOffset -> VCalendar -> Either ICalError [RItem VEvent]
vEventList = vList vcEvents

-- recurrenceList function for VJournal
-- Takes the user's TimeZoneOffset (LocalTime -> Maybe TimeZone)
-- Generally the user should either grab a timezone definition from
-- the vcalendar e.g. (vCalendarTZIDToOffsets vCalendar userTimeZoneIdentifier)
-- use a fixed one e.g. const (Just utc)
vJournalList :: TimeToOffset -> VCalendar -> Either ICalError [RItem VJournal]
vJournalList = vList vcJournals

-- recurrenceList function for VTodo
-- Takes the user's TimeZoneOffset (LocalTime -> Maybe TimeZone)
-- Generally the user should either grab a timezone definition from
-- the vcalendar e.g. (vCalendarTZIDToOffsets vCalendar userTimeZoneIdentifier)
-- use a fixed one e.g. const (Just utc)
vTodoList :: TimeToOffset -> VCalendar -> Either ICalError [RItem VTodo]
vTodoList = vList vcTodos

vList :: VRecurrence r => (VCalendar -> M.Map a r) -> TimeToOffset -> VCalendar -> Either ICalError [RItem r]
vList f ftz vcal = recurrenceList ftz (vCalendarTZIDToOffsets vcal) (M.elems $ f vcal)

recurrenceList' :: HasRRule r => TimeToOffset -> TZIDToOffset -> [r] -> Either ICalError [RItem r]
recurrenceList' ftz ztz rs = heapSort <$> traverseE (rRuleRecurrence ftz ztz <$> rs)

-- The recurrence expansion function. Takes a function to compute the user's timezone
-- A function to compute an arbitrary timezone in the calendar (see vCalendarTZIDToOffsets)
-- and a list of VRecurrence
recurrenceList :: VRecurrence r => TimeToOffset -> TZIDToOffset -> [r] -> Either ICalError [RItem r]
recurrenceList ftz ztz rs = filter ex <$> recurrenceList' ftz ztz rs
  where
    ex (RItem _ _ r) = isJust (vRecurId r) || not (S.member (vInstanceID r) exSet)
    exSet = S.fromList $ vRUID <$> filter (isJust . vRecurId) rs

heapSort :: Ord r => [[r]] -> [r]
heapSort = fromHeapList . H.fromList

fromHeapList :: Ord r => H.MinHeap [r] -> [r]
fromHeapList  heap = case H.view heap of
  Just (i:is, newHeap) -> i : fromHeapList (case is of [] -> newHeap
                                                       xs -> H.insert xs newHeap)
  Just ([], newHeap) -> fromHeapList newHeap
  Nothing -> []


-- Takes a VCalendar and returns a function which given a timezone ID and a time returns a timezone offset.
vCalendarTZIDToOffsets :: VCalendar -> TZIDToOffset -- Text -> LocalTime -> Either ICalError TimeZone
vCalendarTZIDToOffsets vcal tz time = join $ flip vTimeZoneToTimeToOffset time <$> tzs
  where
    tzs :: Either ICalError VTimeZone
    tzs = maybe (icalError "Couldn't find timezone") Right $ M.lookup tz (vcTimeZones vcal)

vTimeZoneToTimeToOffset :: VTimeZone -> TimeToOffset -- LocalTime -> Either ICalError TimeZone
vTimeZoneToTimeToOffset vtz = let
    getTZ :: Either ICalError [RItem TZProp] -> TimeToOffset
    getTZ (Left e) _ = Left e
    getTZ (Right xs) lt = tzPropToTimeZone vtz . rItem <$> findP (cond lt) xs
    findP _ []       = icalError "Unknown timezone offset, empty timezone list"
    findP f [x]      = if f x then icalError "Unknown timezone offset, timezone list terminated before date" else Right x
    findP f (x:y:xs) = if f y then Right x else findP f (y:xs)
    cond lt rtz = case rIStartDateUtc rtz of
      Just t  -> t >= localTimeToUTC utc lt
      Nothing -> False
  in getTZ $ vTimeZoneRecurrence vtz

vTimeZoneRecurrence :: VTimeZone -> Either ICalError [RItem TZProp]
vTimeZoneRecurrence vtz = recurrenceList' fErr zErr $ S.toList (vtzStandardC vtz) ++ S.toList (vtzDaylightC vtz)
  where
    fErr = const (Right utc)
    zErr = const . const $ icalError "TimeZone recurrences shouldn't use a Zoned Date"

tzPropToTimeZone :: VTimeZone -> TZProp -> TimeZone
tzPropToTimeZone vtz prop = TimeZone (utcOffsetValue (tzpTZOffsetTo prop) `div` 60) (error "Timezone Summer Only undefined") (unpack (tzidValue (vtzId vtz)))
-- TODO figuring out a summer only from the TZProp might be difficult without reworking the data structure undefined for now

rRuleRecurrence :: HasRRule r => TimeToOffset -> TZIDToOffset -> r -> Either ICalError [RItem r]
rRuleRecurrence ftz ztz hr = (\i -> filters (heapSort i)) <$> initialList hr
  where
    filters = filterDuplicateSorted  . filter (isException hr)
    toItems :: HasRRule r => [r] -> Either ICalError [RItem r]
    toItems [] = Right []
    toItems (r:rs) = do
      let
        out (Right ri) = ri
        out (Left _) = error "toItem should be unreachable on a tail of a list"
      (start, end) <- vUTCTimes ftz ztz r
      pure $ RItem start end r : out (toItems rs)
    initialList :: HasRRule r => r -> Either ICalError [[RItem r]]
    initialList ri =  if hasRecurrence ri
      then do
        l <- rDateRItemLL ri
        r <- rRuleRItemLL ri
        pure $ l ++ r
      else traverseE $ [toItems [ri]]
    rDateRItemLL :: HasRRule r => r -> Either ICalError [[RItem r]]
    rDateRItemLL ri = traverseE $ rItemRDate <$> join (rDateToList <$> S.toList (vRDate ri))
      where rItemRDate prop = do
              item <- vUpdateBoth ri prop
              toItems [item]
    rRuleRItemLL :: HasRRule r => r -> Either ICalError [[RItem r]]
    rRuleRItemLL ri = traverseE $ rRuleRItemL ri <$> S.toList (vRRule ri)
    rRuleRItemL :: HasRRule r => r -> RRule -> Either ICalError [RItem r]
    rRuleRItemL ri rrule = do
      xs <- rRuleToRRuleFunc rrule ri
      toItems xs

traverseE :: [Either [a] b] -> Either [a] [b]
traverseE [] = Right []
traverseE ((Left as) : xs) = case traverseE xs of
  Right _ -> Left as
  Left bs -> Left (as ++ bs)
traverseE ((Right b) : xs) = (\bs -> b : bs) <$> traverseE xs

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
