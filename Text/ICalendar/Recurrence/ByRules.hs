module Text.ICalendar.Recurrence.ByRules
  (
    rRuleToRRuleFunc
  , filterDuplicateSorted
  , vDateTimeToLocalTime
  )
  where

import           Data.Ix                        (range)
import qualified Data.List                      as L
import           Data.Maybe
import qualified Data.Set                       as S
import           Data.Time                      (Day, LocalTime (..), TimeOfDay,
                                                 addDays,
                                                 addGregorianMonthsClip,
                                                 addGregorianYearsClip,
                                                 addUTCTime, fromGregorian,
                                                 fromGregorianValid,
                                                 gregorianMonthLength,
                                                 isLeapYear, localTimeToUTC,
                                                 midnight, toGregorian, todHour,
                                                 todMin, todSec, utc,
                                                 utcToLocalTime)
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import           Data.Time.Calendar.WeekDate    (toWeekDate)
import           Text.ICalendar.Types

-- Generates a Lazy expansion of an RRule given an initial HasRRule with a start date
-- This is possibly infinite unless there is an Until or Count component specified
rRuleToRRuleFunc :: HasRRule r => RRule -> r -> [r]
rRuleToRRuleFunc rrule r = case vDTStart r of
  Just (DTStart d o) -> vUpdate r . flip DTStart o . updateVDateTime d <$> rRuleToRRuleFunc' rrule (vDateTimeToLocalTime d)
  Nothing -> [r]

vDateTimeToLocalTime :: VDateTime -> LocalTime
vDateTimeToLocalTime (VDate d) = LocalTime (dateValue d) midnight
vDateTimeToLocalTime (VDateTime dt) = uVDT dt
  where
    uVDT (FloatingDateTime lt) = lt
    uVDT (ZonedDateTime lt _)  = lt
    uVDT (UTCDateTime utcT)    =  utcToLocalTime utc utcT

updateVDateTime ::  VDateTime -> LocalTime -> VDateTime
updateVDateTime (VDate _) lt = VDate (Date (localDay lt))
updateVDateTime (VDateTime dt) lt = VDateTime (uVDT dt)
  where
    uVDT (FloatingDateTime _) = FloatingDateTime lt
    uVDT (ZonedDateTime _ tz) = ZonedDateTime lt tz
    uVDT (UTCDateTime _)      =  UTCDateTime $ localTimeToUTC utc lt

-- rRuleToRRuleFunc' generates the underlying recurrences
-- This function is not currently Total. To become total it would 1)
-- Need to return an error structure on invalid RRules. This should probably happen a a higher level
-- In certain cases (invalid RRules and impossible RRules this function can bottom by looping)
-- To fix this it should probably stop if it detrmines that no occurences can be generated.
-- Since there are 7 days of the week plus leap years plus leap year exceptions plus intervals
-- It would need to check cases for 7 * 4 * 2 * Interval Years before it detrmines that this function will lookup
-- E.g. byMonth 2 byMonthDay 31 is a valid RRule but will loop.
rRuleToRRuleFunc' :: RRule -> LocalTime -> [LocalTime]
rRuleToRRuleFunc' rrule r = (untilComponent r . byComponent . freqComponent) r
  where recur = rRuleValue rrule
        -- Because of the way bySetPos needs to expand filters the by Components
        -- can start before the initial date so we need to drop previous dates
        untilComponent elm = rUntilCount recur . dropWhile (< elm)
        freqComponent = interval recur . freq recur
        byComponent = bySetPos recur byRules
        byRules =  bySecond recur . byMinute recur . byHour recur . byRulesDay
        byRulesDay' = foldr1 (.) $ (\x -> x recur) <$>
                    -- Some cases of the by*Day over expand the days and need to be refiltered.
                    [ byWeekNoFilter, byMonthFilter
                    , byDay, byMonthDay, byYearDay, byWeekNo, byMonth ]
        -- Sort and unique because some edge cases of byrule combinations can create duplicates
        byRulesDay = foldr ((++) . sortUniq . byRulesDay' . pure) []

freq :: Recur -> LocalTime -> [LocalTime]
freq recur r = r : freq recur rnew
  where
    rnew = ( case recurFreq recur of
      Secondly -> addSeconds (1 :: Integer)
      Minutely -> addSeconds (60 :: Integer)
      Hourly   -> addSeconds (3600 :: Integer)
      Daily    -> updateDay (addDays 1)
      Weekly   -> updateDay (addDays 7)
      -- Technically clipping the month is incorrect. Dates which are invalid should be ignored
      -- However the ByRules need to expand before removing invalid dates. This really should be
      -- accomplished by storing the date in a Y M D datastructure and remove invalid entries as a post process.
      -- Day (The day component of LocalTime) is just an Integer type from a specific
      -- day so these methods would need to use a seperate datatype. For that reason be are not going
      -- to expose most functions in this module since the signatures aren't stable.
      Monthly  -> updateDay (addGregorianMonthsClip 1)
      Yearly   -> updateDay (addGregorianYearsClip 1)
      ) r

interval :: Recur -> [LocalTime] -> [LocalTime]
interval r (x:xs) = x : interval r (drop (recurInterval r - 1) xs)
interval _ []     = []

-- bySetPos grabs the nth (and -nth) parts of the by components
-- Since that is the case we need to seperate the freq component and
-- the by components to count how many to take
bySetPos :: Recur -> ([LocalTime] -> [LocalTime]) -> [LocalTime] -> [LocalTime]
bySetPos recur byf = case recurBySetPos recur of
    [] -> byf
    is -> foldr bySetPosF []
      where
        bySetPosF r = (++) $ getIndexes (byf [r])
        getIndexes ys = fst $ unzip $ filter (\(_, y) -> S.member y (indexes ys)) (zip ys [0..])
        indexes ys = S.fromList $ toIndex ys <$> is
        toIndex ys i | i >= 0 = i - 1 -- Zero index
                     | otherwise = i + length ys -- Negative index
byMonth :: Recur -> [LocalTime] -> [LocalTime]
byMonth recur
  | null (recurByMonth recur) = id
  | recurFreq recur <= Monthly = byMonthFilterF recur
  | otherwise = concatMap (\r -> flip updateDay r <$> (const <$> newDays r))
      where
        newDays r = L.sort $ catMaybes $ toMonth (localDay r) <$> recurByMonth recur
        toMonth d i = fromGregorianValid year i day
          where
            (year, _, day) = toGregorian d

byMonthFilterF :: Recur -> [LocalTime] -> [LocalTime]
byMonthFilterF recur = filterDay $ \d ->
  let (_, month, _) = toGregorian d
  in month `elem` recurByMonth recur

byMonthFilter :: Recur -> [LocalTime] -> [LocalTime]
byMonthFilter recur
  | not (null (recurByMonth recur)) &&
    (recurFreq recur == Weekly || recurFreq recur == Yearly)
    = byMonthFilterF recur
  | otherwise = id

byWeekNo :: Recur -> [LocalTime] -> [LocalTime]
byWeekNo recur
  | null (recurByWeekNo recur) = id
  | recurFreq recur <= Weekly = byWeekNoFilterF recur -- Technically not valid need this function anyways.
  | recurFreq recur == Yearly = expandWeekNo (recurWkSt recur) (recurByWeekNo recur)
  | otherwise = error $ "BYWEEKNO is only Valid for Yearly Rules. Not " ++
      show (recurFreq recur) ++ " for " ++ show recur


expandWeekNo :: Integral a => Weekday -> [a] -> [LocalTime] -> [LocalTime]
expandWeekNo wk recurL = updateDays $ filterDuplicateSorted . L.sort . catMaybes . (\d -> toDay d <$> recurL)
  where toDay d i = let
                      numWeeks = weekNoLength wk d
                      week = weekNo wk d
                      pos = 7 * (fromIntegral i - week)
                      neg = 7 * (fromIntegral i + numWeeks - week + 1)
                      day = if i > 0 then addDays pos d else addDays neg d
                      (a, b) = yearIter d
                      in if day >= a && day <= b then Just day else Nothing

byWeekNoFilterF :: Recur -> [LocalTime] -> [LocalTime]
byWeekNoFilterF recur = filterDay $ \d ->
  let
    xs = recurByWeekNo recur
    wk = recurWkSt recur
    numWeeks = weekNoLength wk d
    week = weekNo wk d
  in week `elem` xs || (week - numWeeks - 1) `elem` xs

weekNo :: Integral a => Weekday -> Day -> a
weekNo wk d = 1 + ((fromIntegral day - 1 - weekNoStartOffset wk d) `div` 7)
  where
    (_, day) = toOrdinalDate d

weekNoStartOffset :: Integral a => Weekday -> Day -> a
weekNoStartOffset wk d = if offset >= -3 then offset else offset + 7
  where
    curWk = fromEnum $ toWeekDay firstDay
    wkSt = fromEnum wk
    offset = fromIntegral $ negate $ (curWk - wkSt) `mod` 7
    firstDay = fromOrdinalDate year 1
    (year, _) = toOrdinalDate d

weekNoLength :: Integral a => Weekday -> Day -> a
weekNoLength wk d = if offset >= (if isLeapYear year then 3 else 2) then 53 else 52
  where
    offset = weekNoStartOffset wk d :: Int
    (year, _) = toOrdinalDate d

yearLength :: (Integral a, Integral b) => a -> b
yearLength year = if isLeapYear (fromIntegral year) then 366 else 365

byWeekNoFilter :: Recur -> [LocalTime] -> [LocalTime]
byWeekNoFilter recur
  | not (null (recurByWeekNo recur)) && recurFreq recur == Yearly = byWeekNoFilterF recur
  | otherwise = id

byYearDay :: Recur -> [LocalTime] -> [LocalTime]
byYearDay recur
  | null (recurByYearDay recur) = id
  | recurFreq recur <= Daily = filterDay f
  | recurFreq recur == Weekly = error $ "BYYEARDAY can't be used with a Weekly rule for " ++ show recur
  | recurFreq recur == Monthly = error $ "BYYEARDAY can't be used with a Monthly rule for " ++ show recur
  | recurFreq recur == Yearly = expandDay (recurByYearDay recur) yearIter
  | otherwise = error "Not reachable"
      where
        f d = day `elem` xs || dayAlt `elem` xs
          where
            xs = recurByYearDay recur
            dayAlt = day - yearLength year - 1
            (year, day) = toOrdinalDate d

byMonthDay :: Recur -> [LocalTime] -> [LocalTime]
byMonthDay recur
  | null (recurByMonthDay recur) = id
  | recurFreq recur <= Daily = filterDay f
  | not (null (recurByYearDay recur)) = filterDay f
  | recurFreq recur == Weekly = error $ "BYMONTHDAY can't be used with a Weekly rule for " ++ show recur
  | recurFreq recur == Monthly = expandDay (recurByMonthDay recur) monthIter
  | recurFreq recur == Yearly = expandDay (recurByMonthDay recur) monthIter
  | otherwise = error "Not reachable"
      where
        f d = day `elem` xs || dayAlt `elem` xs
          where
            xs = recurByMonthDay recur
            dayAlt = day - gregorianMonthLength year month - 1
            (year, month, day) = toGregorian d


expandDay :: Integral a => [a] -> (Day -> (Day, Day)) -> [LocalTime] -> [LocalTime]
expandDay recurL iter = updateDays $ filterDuplicateSorted . L.sort . catMaybes . (\d -> toDay d <$> recurL)
  where toDay d i = let
          (a, b) = iter d
          day = if i > 0 then addDays (fromIntegral i - 1) a else addDays (fromIntegral i + 1) b
          in if day >= a && day <= b then Just day else Nothing

byDay :: Recur -> [LocalTime] -> [LocalTime]
byDay recur
  | null (recurByDay recur) = id
  | recurFreq recur <= Daily = filterDay f
  | not (null (recurByYearDay recur)) = filterDay f
  | not (null (recurByMonthDay recur)) = filterDay f
  | recurFreq recur == Weekly = expand (weekIter (recurWkSt recur))
  | recurFreq recur == Yearly && not (null (recurByWeekNo recur)) = expand (weekIter (recurWkSt recur))
  | monthly = expand monthIter
  | recurFreq recur == Yearly = expand yearIter
  | otherwise = error "Not reachable"
    where
      -- This could probably be more efficient espically in the case of one Int Wkday pair
      expand :: (Day -> (Day, Day)) -> [LocalTime] -> [LocalTime]
      expand df -- for performance if we don't have every weekday entries just generate output rather than expanding and filtering
       | null daySet = updateDays (filterDuplicateSorted . L.sort . catMaybes . (\d -> toDay d <$> S.elems dayNumSet))
       | otherwise = updateDays (filter f . range . df)
        where
          toDay :: Day -> (Weekday, Int) -> Maybe Day
          toDay d (wk, i)
            | i > 0 = if pos <= b then Just pos else Nothing
            | i < 0 = if neg >= a then Just neg else Nothing
            | otherwise = Nothing
            where
              pos = addDays (fromIntegral $ (i - 1) * 7 + ((fromEnum wk - fromEnum (toWeekDay a)) `mod` 7)) a
              neg = addDays (fromIntegral $ (i + 1) * 7 - ((fromEnum (toWeekDay b) - fromEnum wk) `mod` 7)) b
              (a, b) = df d
      f r = S.member (toWeekDay r) daySet || f'
        where
          f'
            | monthly = let
                (year, month, day) = toGregorian r
                monthLength = gregorianMonthLength year month
                in hasDate monthLength day
            | recurFreq recur == Yearly = let
              (year, day) = toOrdinalDate r
              in hasDate (yearLength year) day
            | otherwise = False
          isMember i = S.member (toWeekDay r, i) dayNumSet
          hasDate len day = isMember (nthWeek (day - 1)) || isMember (negate (nthWeek (len - day)))
      monthly = (recurFreq recur == Monthly) || not (null (recurByMonth recur))
      (daySet, dayNumSet) = foldr foldf (S.empty, S.empty) (recurByDay recur)
        where foldf (Right wd) (b, c)     = (S.insert wd b, c)
              foldf (Left (i, wd)) (b, c) = (b, S.insert (wd, i) c)

weekIter :: Weekday -> Day -> (Day, Day)
weekIter wk d = let
  curWk = fromEnum $ toWeekDay d
  wkSt = fromEnum wk
  offset = fromIntegral $ (curWk - wkSt) `mod` 7
  in (addDays (negate offset) d, addDays (6 - offset) d)

monthIter :: Day -> (Day, Day)
monthIter d = (fromGregorian year month 1, fromGregorian year month monthLength)
  where
    (year, month, _) = toGregorian d
    monthLength = gregorianMonthLength year month

yearIter :: Day -> (Day, Day)
yearIter d = (fromOrdinalDate year 1, fromOrdinalDate year (yearLength year))
  where
    (year, _) = toOrdinalDate d

toWeekDay :: Day -> Weekday
toWeekDay d = toEnum (i `mod` 7)
  where (_, _ , i) = toWeekDate d

-- Givin a day (0 indexed) calculate the nth week (not zero indexed)
nthWeek :: Integral a => a -> a
nthWeek day = (day `div` 7) + 1

byHour :: Recur -> [LocalTime] -> [LocalTime]
byHour = _byTime 24 todHour (\t i -> t { todHour = i}) recurByHour Hourly

byMinute :: Recur -> [LocalTime] -> [LocalTime]
byMinute = _byTime 60 todMin (\t i -> t { todMin = i}) recurByMinute Minutely

bySecond :: Recur -> [LocalTime] -> [LocalTime]
bySecond = _byTime 60 (floor . todSec) (\t i -> t { todSec = fromIntegral i}) recurBySecond Secondly

_byTime :: Integral a => a -> (TimeOfDay -> a) -> (TimeOfDay -> a -> TimeOfDay) -> (Recur -> [a]) -> Freq -> Recur -> [LocalTime] -> [LocalTime]
_byTime upperBound getT updateT recurF freqEnum recur
  | null recurL = id
  | recurFreq recur <= freqEnum = filterTime (\t -> getT t `elem` recurL)
  | otherwise = updateTimes (\r -> updateT r <$> recurL)
  where
    recurL = L.sort $ filter (\i -> i >= 0 && i < upperBound) (recurF recur)

rUntilCount :: Recur -> [LocalTime] -> [LocalTime]
rUntilCount recur = case recurUntilCount recur of
  Nothing               -> id
  Just (Left (Left d))  -> takeWhile ((<=) (dateValue d) . localDay)
  Just (Left (Right d)) -> takeWhile (dateTimeToLocalTime d >=)
  Just (Right i)        -> take i

sortUniq :: Ord a => [a] -> [a]
sortUniq = filterDuplicateSorted . L.sort

filterDuplicateSorted :: Eq a => [a] -> [a]
filterDuplicateSorted [] = []
filterDuplicateSorted [x] = [x]
filterDuplicateSorted (x:xs) = if x == head xs then new_xs else x:new_xs
  where new_xs = filterDuplicateSorted xs

-- Short helper functions

filterDay :: (Day -> Bool) -> [LocalTime] -> [LocalTime]
filterDay f = filter $ f . localDay

filterTime :: (TimeOfDay -> Bool) -> [LocalTime] -> [LocalTime]
filterTime f = filter $ f . localTimeOfDay


updateTimes :: (TimeOfDay -> [TimeOfDay]) -> [LocalTime]  -> [LocalTime]
updateTimes f = concatMap $ \(LocalTime d t) -> LocalTime d <$> f t

updateDay :: (Day -> Day) -> LocalTime -> LocalTime
updateDay f (LocalTime d t) = LocalTime (f d) t

updateDays :: (Day -> [Day]) ->  [LocalTime] -> [LocalTime]
updateDays f = concatMap $ \(LocalTime d t) -> (`LocalTime` t) <$> f d

dateTimeToLocalTime :: DateTime -> LocalTime
dateTimeToLocalTime (FloatingDateTime dt) = dt
dateTimeToLocalTime (ZonedDateTime dt _)  = dt
dateTimeToLocalTime (UTCDateTime dt)      =  utcToLocalTime utc dt

addSeconds :: Integral a => a -> LocalTime -> LocalTime
addSeconds i time = utcToLocalTime utc (addUTCTime (fromIntegral i) (localTimeToUTC utc time))
