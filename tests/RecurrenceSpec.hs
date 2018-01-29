{-# LANGUAGE OverloadedStrings #-}

module RecurrenceSpec where
import           Data.ByteString.Lazy              (ByteString)
import qualified Data.ByteString.Lazy              as BS

import qualified Data.Map                          as M
import           Data.Maybe
import qualified Data.Set                          as S


import           Control.Monad                     (join)
import           Data.Default
import           Data.Time                         (LocalTime (..),
                                                    TimeOfDay (..),
                                                    TimeZone (..),
                                                    ZonedTime (..),
                                                    fromGregorian,
                                                    zonedTimeToUTC)
import           Test.Hspec
import           Text.Parsec

import           Text.ICalendar
import           Text.ICalendar.Recurrence.ByRules

import           Paths_iCalendar

data RecurTest = RecurTest {
    rtDescription :: String
  , rtRRRule      :: RecurTestData
  , rtResults     :: [ByteString] -- Results as in ical spec. Needs to be parsed
  , rtStrict      :: Bool -- Recurrence has finite results
}

data RecurTestData = RRuleTestData {
    rtDTStart :: ByteString
  , rtRRules  :: [ByteString] -- Possible RRules (can have exdate)
  } | VTimeZoneTestData {
    rtVTimeZone :: ByteString
  } | VCalendarTestData {
    rtCalendarPath :: FilePath
  }

-- Spec Test Runner
spec :: Spec
spec = do
  describe "VTIMEZONE" $ toTests timeZoneTests
  describe "RRULE" $ toTests recurTests
  describe "ICS Tests" $ toTests gCalTests
  where toTests ts = foldr1 (>>) (map rruleTest ts)


--Utility functions for running tests
right :: Show a => Either a b -> b
right (Left a)  = error $ show a
right (Right b) = b

ical :: ByteString -> VCalendar
ical vcal = head . fst . right $ parseICalendar def vcal

numEntries :: Int
numEntries = 300

rruleTest :: RecurTest -> Spec
rruleTest (RecurTest desc icalData resultsL strict) = do
  icalText <- runIO $ recurTestDataToIcal icalData
  context desc $ foldr1 (>>) (specR icalData <$> icalText)
  where
    specR (RRuleTestData _ _) rule = do
      it "rRule matches dates" $ take (length resultsLT) (outRecurF rule) `shouldBe` resultsLT
      dateMatchTest (outEventsF rule)
      where
        outRecurF =  map (vDateTimeToLocalTime . dtStartValue) . rruleF . head . M.elems . vcEvents . ical
        rruleF r = catMaybes $ vDTStart <$> rRuleToRRuleFunc (head $ S.elems $ vRRule r) r
    specR (VTimeZoneTestData _) rule = dateMatchTest (outTimezoneF rule)
    specR (VCalendarTestData _) rule = dateMatchTest (outEventsF rule)
    dateMatchTest eventsList = do
      it "matches dates" $
        take (length resultsUTC) eventsList  `shouldBe` resultsUTC
      it "terminates or not terminates" $
        length (take numEntries eventsList) `shouldBe`
          if strict then length resultsUTC else numEntries
    outEventsF = mapMaybe rIStartDateUtc . vEventList undefined . ical
    outTimezoneF = mapMaybe rIStartDateUtc . vTimeZoneRecurrence . head . M.elems . vcTimeZones . ical
    results = foldl1 (++) (right . parseDates <$> resultsL)
    resultsLT = zonedTimeToLocalTime <$> results
    resultsUTC = zonedTimeToUTC <$> results

recurTestDataToIcal :: RecurTestData -> IO [ByteString]
recurTestDataToIcal (RRuleTestData dtstart rules) = let
    toBS rule = vCalText $ BS.intercalate "\n" [vTimeZone, vEventText $ BS.intercalate "\n" [dtstart, rule]]
  in pure $ map toBS rules
recurTestDataToIcal (VTimeZoneTestData timezone) = pure [vCalText timezone]
recurTestDataToIcal (VCalendarTestData d) = do
  path <- getDataFileName d
  iCalText <- BS.readFile path
  pure [iCalText]

vCalText :: ByteString -> ByteString
vCalText s = BS.intercalate "\n" ["BEGIN:VCALENDAR"
  , "VERSION:2.0", "PRODID:-//hacksw/handcal//NONSGML v1.0//EN"
  , s, "END:VCALENDAR"]

vEventText :: ByteString -> ByteString
vEventText s = BS.intercalate "\n" ["BEGIN:VEVENT", "DTSTAMP:19980309T231000Z"
  ,"UID:guid-1.host1.com", s, "SUMMARY:Bastille Day Party", "END:VEVENT"]


-- Test data from an ical file.
gCalTests :: [RecurTest]
gCalTests = [
  RecurTest "Delete Event"
    (VCalendarTestData "tests/ical/delete.ics")
    ["(2016 14:00 AM EDT)September 22,23,25,26"]
    True
  , RecurTest "Change Event"
    (VCalendarTestData "tests/ical/change.ics")
    ["(2016 14:00 AM EDT)September 22"
    , "(2016 13:30 AM EDT)September 23"
    , "(2016 14:00 AM EDT)September 23,25,26"]
    True
  , RecurTest "Multiple Events"
    (VCalendarTestData "tests/ical/full.ics")
    ["(September 22, 2016 EDT)14:00"
    , "(September 23, 2016 EDT)13:00,14:00,15:00"
    , "(September 24, 2016 EDT)13:00,14:00"
    , "(September 25, 2016 EDT)14:00,16:00"
    , "(September 26, 2016 EDT)13:00,14:00"
    , "(September 27, 2016 EDT)13:00"]
    True
  ]

-- Time Zone Test Data
timeZoneTests :: [RecurTest]
timeZoneTests = [
  RecurTest "New York Time Zone"
    (VTimeZoneTestData vTimeZone)
    (startTimes ++ ["(1998 2:00 AM UTC)April 05;October 25"])
    False
  , RecurTest "Test Timezone with RDate"
    (VTimeZoneTestData vTimeZoneRDate)
    ["(1997 2:00 AM UTC)April 6;October 26"]
    True
  , RecurTest "Test DST Time Zone"
    (VTimeZoneTestData vTimeZoneEnd)
    (startTimes ++ ["(1998 2:00 AM UTC)October 25"])
    False
  , RecurTest "Test Timezone with 2 Recurs"
    (VTimeZoneTestData vTimeZoneEndStart)
    (startTimes ++ ["(1998 2:00 AM UTC)April 05;October 25"
    , "(1999 2:00 AM UTC)April 25;October 31"])
    False
  ]
  where
    startTimes = [ "(1967 2:00 AM UTC)October 29"
      , "(1968 2:00 AM UTC)October 27"
      , "(1969 2:00 AM UTC)October 26"
      , "(1970 2:00 AM UTC)October 25"
      , "(1971 2:00 AM UTC)October 31"
      , "(1972 2:00 AM UTC)October 29"
      , "(1973 2:00 AM UTC)October 28"
      , "(1974 2:00 AM UTC)October 27"
      , "(1975 2:00 AM UTC)October 26"
      , "(1976 2:00 AM UTC)October 31"
      , "(1977 2:00 AM UTC)October 30"
      , "(1978 2:00 AM UTC)October 29"
      , "(1979 2:00 AM UTC)October 28"
      , "(1980 2:00 AM UTC)October 26"
      , "(1981 2:00 AM UTC)October 25"
      , "(1982 2:00 AM UTC)October 31"
      , "(1983 2:00 AM UTC)October 30"
      , "(1984 2:00 AM UTC)October 28"
      , "(1985 2:00 AM UTC)October 27"
      , "(1986 2:00 AM UTC)October 26"
      , "(1987 2:00 AM UTC)April 05;October 25"
      , "(1988 2:00 AM UTC)April 03;October 30"
      , "(1989 2:00 AM UTC)April 02;October 29"
      , "(1990 2:00 AM UTC)April 01;October 28"
      , "(1991 2:00 AM UTC)April 07;October 27"
      , "(1992 2:00 AM UTC)April 05;October 25"
      , "(1993 2:00 AM UTC)April 04;October 31"
      , "(1994 2:00 AM UTC)April 03;October 30"
      , "(1995 2:00 AM UTC)April 02;October 29"
      , "(1996 2:00 AM UTC)April 07;October 27"
      , "(1997 2:00 AM UTC)April 06;October 26"
      ]

-- These are the VTimeZone tests in the icalendar spec.
vTimeZone, vTimeZoneRDate, vTimeZoneEnd, vTimeZoneEndStart :: ByteString -- Timezone spec
vTimeZone = "BEGIN:VTIMEZONE\n\
  \TZID:US-Eastern\n\
  \LAST-MODIFIED:19870101T000000Z\n\
  \TZURL:http://zones.stds_r_us.net/tz/US-Eastern\n\
  \BEGIN:STANDARD\n\
  \DTSTART:19671029T020000\n\
  \RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10\n\
  \TZOFFSETFROM:-0400\n\
  \TZOFFSETTO:-0500\n\
  \TZNAME:EST\n\
  \END:STANDARD\n\
  \BEGIN:DAYLIGHT\n\
  \DTSTART:19870405T020000\n\
  \RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4\n\
  \TZOFFSETFROM:-0500\n\
  \TZOFFSETTO:-0400\n\
  \TZNAME:EDT\n\
  \END:DAYLIGHT\n\
  \END:VTIMEZONE"

vTimeZoneRDate = "BEGIN:VTIMEZONE\n\
  \TZID:US-Eastern\n\
  \LAST-MODIFIED:19870101T000000Z\n\
  \BEGIN:STANDARD\n\
  \DTSTART:19971026T020000\n\
  \RDATE:19971026T020000\n\
  \TZOFFSETFROM:-0400\n\
  \TZOFFSETTO:-0500\n\
  \TZNAME:EST\n\
  \END:STANDARD\n\
  \BEGIN:DAYLIGHT\n\
  \DTSTART:19971026T020000\n\
  \RDATE:19970406T020000\n\
  \TZOFFSETFROM:-0500\n\
  \TZOFFSETTO:-0400\n\
  \TZNAME:EDT\n\
  \END:DAYLIGHT\n\
  \END:VTIMEZONE"

vTimeZoneEnd = "BEGIN:VTIMEZONE\n\
  \TZID:US--Fictitious-Eastern\n\
  \LAST-MODIFIED:19870101T000000Z\n\
  \BEGIN:STANDARD\n\
  \DTSTART:19671029T020000\n\
  \RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10\n\
  \TZOFFSETFROM:-0400\n\
  \TZOFFSETTO:-0500\n\
  \TZNAME:EST\n\
  \END:STANDARD\n\
  \BEGIN:DAYLIGHT\n\
  \DTSTART:19870405T020000\n\
  \RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980404T070000Z\n\
  \TZOFFSETFROM:-0500\n\
  \TZOFFSETTO:-0400\n\
  \TZNAME:EDT\n\
  \END:DAYLIGHT"

vTimeZoneEndStart = "BEGIN:VTIMEZONE\n\
  \TZID:US--Fictitious-Eastern\n\
  \LAST-MODIFIED:19870101T000000Z\n\
  \BEGIN:STANDARD\n\
  \DTSTART:19671029T020000\n\
  \RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10\n\
  \TZOFFSETFROM:-0400\n\
  \TZOFFSETTO:-0500\n\
  \TZNAME:EST\n\
  \END:STANDARD\n\
  \BEGIN:DAYLIGHT\n\
  \DTSTART:19870405T020000\n\
  \RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=4;UNTIL=19980504T070000Z\n\
  \TZOFFSETFROM:-0500\n\
  \TZOFFSETTO:-0400\n\
  \TZNAME:EDT\n\
  \END:DAYLIGHT\n\
  \BEGIN:DAYLIGHT\n\
  \DTSTART:19990424T020000\n\
  \RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=4\n\
  \TZOFFSETFROM:-0500\n\
  \TZOFFSETTO:-0400\n\
  \TZNAME:EDT\n\
  \END:DAYLIGHT\n\
  \END:VTIMEZONE"

-- Date Parser for the data format listed in the ical spec
parseDates :: ByteString -> Either ParseError [ZonedTime]
parseDates = parse (try dateParser <|> try timeParser) "parse dates"
  where
    timezone :: Parsec ByteString () TimeZone
    timezone = do
      tz <- try (string "EST") <|> try (string "EDT") <|> string "UTC"
      pure $ case tz of
        "EST" -> TimeZone (-4 * 60) False "EST"
        "EDT" -> TimeZone (-5 * 60) False "EDT"
        "UTC" -> TimeZone 0 False "UTC"
        _     -> error "Not Reachable"
    year :: Parsec ByteString () Integer
    year = read <$> count 4 digit
    time :: Parsec ByteString () TimeOfDay
    time = do
      h <- hour
      m <- char ':' *> minute
      pure $ TimeOfDay h m 0
    hour :: Parsec ByteString () Int
    hour = read <$> many1 digit
    day :: Parsec ByteString () Int
    day = read <$> many1 digit
    minute :: Parsec ByteString () Int
    minute = read <$> count 2 digit
    month :: Parsec ByteString () Int
    month = do
      m <- many letter
      pure $ case m of
        "January"   -> 1
        "February"  -> 2
        "March"     -> 3
        "April"     -> 4
        "May"       -> 5
        "June"      -> 6
        "July"      -> 7
        "August"    -> 8
        "September" -> 9
        "October"   -> 10
        "November"  -> 11
        "December"  -> 12
        _           -> error "Invalid Month"
    dayMonths :: (Int -> Int -> a) -> Parsec ByteString () [[a]]
    dayMonths toZoned = flip sepBy1 (char ';') $ do
      m <- month <* space
      ds <- days
      pure $ toZoned m <$> ds
    days :: Parsec ByteString () [Int]
    days = try days1 <|> sepBy1 day (char ',')
      where
        days1 = do
          d1 <- day
          d2 <- char '-' *> day
          pure [d1..d2]
    dateParser :: Parsec ByteString () [ZonedTime]
    dateParser = do
      y <- char '(' *> year
      t <- space *> time
      tz <- string " AM " *> timezone
      dss <- char ')' *> dayMonths (\m d -> ZonedTime (LocalTime (fromGregorian y m d) t) tz)
      pure $ join dss
    timeParser = do
        m <- string "(" *> month
        d <- space *> day
        y <- string ", " *> year
        tz <- space *> timezone <* char ')'
        ts <- sepBy1 time (char ',')
        pure $ (\t -> ZonedTime (LocalTime (fromGregorian y m d) t) tz) <$> ts

-- These are the example in the Icalender RFC as defined in the RFC.
-- This should test the by-rules as written by the icalendar spec
-- The don't really test unusual combinations of by rules for which
-- the results might be unclear (e.g. byMonth and byWeekNo).
recurTests :: [RecurTest]
recurTests = [
  RecurTest "Daily for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=DAILY;COUNT=10"])
    ["(1997 9:00 AM EDT)September 2-11"]
    True
  , RecurTest "Daily until December 24, 1997:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=DAILY;UNTIL=19971224T000000Z"])
    ["(1997 9:00 AM EDT)September 2-30;October 1-25"
    , "(1997 9:00 AM EST)October 26-31;November 1-30;December 1-23"]
    True
  , RecurTest "Every other day - forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=DAILY;INTERVAL=2"])
    ["(1997 9:00 AM EDT)September 2,4,6,8,10,12,14,16,18,20,22,24,26,28,30;October 2,4,6,8,10,12,14,16,18,20,22,24"
    , "(1997 9:00 AM EST)October 26,28,30;November 1,3,5,7,9,11,13,15,17,19,21,23,25,27,29;December 1,3"]
    False
  , RecurTest "Every 10 days, 5 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5"])
    ["(1997 9:00 AM EDT)September 2,12,22;October 2,12"]
    True
  , RecurTest "Everyday in January, for 3 years:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19980101T090000"
      ["RRULE:FREQ=YEARLY;UNTIL=20000131T090000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA"
      , "RRULE:FREQ=DAILY;UNTIL=20000131T090000Z;BYMONTH=1"])
    ["(1998 9:00 AM EST)January 1-31"
    , "(1999 9:00 AM EST)January 1-31"
    , "(2000 9:00 AM EST)January 1-31"]
    True
  , RecurTest "Weekly for 10 occurrences"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=WEEKLY;COUNT=10"])
    ["(1997 9:00 AM EDT)September 2,9,16,23,30;October 7,14,21"
    , "(1997 9:00 AM EST)October 28;November 4"]
    True
  , RecurTest "Weekly until December 24, 1997"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z"])
    ["(1997 9:00 AM EDT)September 2,9,16,23,30;October 7,14,21"
    , "(1997 9:00 AM EST)October 28;November 4,11,18,25;December 2,9,16,23"]
    True
  , RecurTest "Every other week - forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU"])
    ["(1997 9:00 AM EDT)September 2,16,30;October 14"
    , "(1997 9:00 AM EST)October 28;November 11,25;December 9,23"
    , "(1998 9:00 AM EST)January 6,20"]
    False
  , RecurTest "Weekly on Tuesday and Thursday for 5 weeks:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH"
      , "RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH"])
    ["(1997 9:00 AM EDT)September 2,4,9,11,16,18,23,25,30;October 2"]
    True
  , RecurTest "Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970901T090000"
      ["RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR"])
    ["(1997 9:00 AM EDT)September 1,3,5,15,17,19,29;October 1,3,13,15,17"
    , "(1997 9:00 AM EST)October 27,29,31;November 10,12,14,24,26,28;December 8,10,12,22"]
    True
  , RecurTest "Every other week on Tuesday and Thursday, for 8 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH"])
    ["(1997 9:00 AM EDT)September 2,4,16,18,30;October 2,14,16"]
    True
  , RecurTest "Monthly on the 1st Friday for ten occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970905T090000"
      ["RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR"])
    ["(1997 9:00 AM EDT)September 5;October 3"
    , "(1997 9:00 AM EST)November 7;December 5"
    , "(1998 9:00 AM EST)January 2;February 6;March 6;April 3"
    , "(1998 9:00 AM EDT)May 1;June 5"]
    True
  , RecurTest "Monthly on the 1st Friday until December 24, 1997:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970905T090000"
      ["RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR"])
    ["(1997 9:00 AM EDT)September 5;October 3"
    , "(1997 9:00 AM EST)November 7;December 5"]
    True
  , RecurTest "Every other month on the 1st and last Sunday of the month for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970907T090000"
      ["RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU"])
    ["(1997 9:00 AM EDT)September 7,28"
    , "(1997 9:00 AM EST)November 2,30"
    , "(1998 9:00 AM EST)January 4,25;March 1,29"
    , "(1998 9:00 AM EDT)May 3,31"]
    True
  , RecurTest "Monthly on the second to last Monday of the month for 6 months:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970922T090000"
      ["RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO"])
    ["(1997 9:00 AM EDT)September 22;October 20"
    , "(1997 9:00 AM EST)November 17;December 22"
    , "(1998 9:00 AM EST)January 19;February 16"]
    True
  , RecurTest "Monthly on the third to the last day of the month, forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970928T090000"
      ["RRULE:FREQ=MONTHLY;BYMONTHDAY=-3"])
    ["(1997 9:00 AM EDT)September 28"
    , "(1997 9:00 AM EST)October 29;November 28;December 29"
    , "(1998 9:00 AM EST)January 29;February 26"]
    False
  , RecurTest "Monthly on the 2nd and 15th of the month for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15"])
    ["(1997 9:00 AM EDT)September 2,15;October 2,15"
    , "(1997 9:00 AM EST)November 2,15;December 2,15"
    , "(1998 9:00 AM EST)January 2,15"]
    True
  , RecurTest "Monthly on the first and last day of the month for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970930T090000"
      ["RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1"])
    ["(1997 9:00 AM EDT)September 30;October 1"
    , "(1997 9:00 AM EST)October 31;November 1,30;December 1,31"
    , "(1998 9:00 AM EST)January 1,31;February 1"]
    True
  , RecurTest "Every 18 months on the 10th thru 15th of the month for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970910T090000"
      ["RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15"])
    ["(1997 9:00 AM EDT)September 10,11,12,13,14,15"
    , "(1999 9:00 AM EST)March 10,11,12,13"]
    True
  , RecurTest "Every Tuesday, every other month:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU"])
    ["(1997 9:00 AM EDT)September 2,9,16,23,30"
    , "(1997 9:00 AM EST)November 4,11,18,25"
    , "(1998 9:00 AM EST)January 6,13,20,27;March 3,10,17,24,31"]
    False
  , RecurTest "Yearly in June and July for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970610T090000"
      ["RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7"])
    ["(1997 9:00 AM EDT)June 10;July 10"
    , "(1998 9:00 AM EDT)June 10;July 10"
    , "(1999 9:00 AM EDT)June 10;July 10"
    , "(2000 9:00 AM EDT)June 10;July 10"
    , "(2001 9:00 AM EDT)June 10;July 10"]
    True
  , RecurTest "Every other year on January, February, and March for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970310T090000"
      ["RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3"])
    ["(1997 9:00 AM EST)March 10"
    , "(1999 9:00 AM EST)January 10;February 10;March 10"
    , "(2001 9:00 AM EST)January 10;February 10;March 10"
    , "(2003 9:00 AM EST)January 10;February 10;March 10"]
    True
  , RecurTest "Every 3rd year on the 1st, 100th and 200th day for 10 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970101T090000"
      ["RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200"])
    ["(1997 9:00 AM EST)January 1"
    , "(1997 9:00 AM EDT)April 10;July 19"
    , "(2000 9:00 AM EST)January 1"
    , "(2000 9:00 AM EDT)April 9;July 18"
    , "(2003 9:00 AM EST)January 1"
    , "(2003 9:00 AM EDT)April 10;July 19"
    , "(2006 9:00 AM EST)January 1"]
    True
  , RecurTest "Every 20th Monday of the year, forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970519T090000"
      ["RRULE:FREQ=YEARLY;BYDAY=20MO"])
    ["(1997 9:00 AM EDT)May 19"
    , "(1998 9:00 AM EDT)May 18"
    , "(1999 9:00 AM EDT)May 17"]
    False
  , RecurTest "Monday of week number 20 (where the default start of the week is Monday), forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970512T090000"
      ["RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO"])
    ["(1997 9:00 AM EDT)May 12"
    , "(1998 9:00 AM EDT)May 11"
    , "(1999 9:00 AM EDT)May 17"]
    False
  , RecurTest "Every Thursday in March, forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970313T090000"
      ["RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH"])
    ["(1997 9:00 AM EST)March 13,20,27"
    , "(1998 9:00 AM EST)March 5,12,19,26"
    , "(1999 9:00 AM EST)March 4,11,18,25"]
    False
  , RecurTest "Every Thursday, but only during June, July, and August, forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970605T090000"
      ["RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8"])
    ["(1997 9:00 AM EDT)June 5,12,19,26;July 3,10,17,24,31;August 7,14,21,28"
    , "(1998 9:00 AM EDT)June 4,11,18,25;July 2,9,16,23,30;August 6,13,20,27"
    , "(1999 9:00 AM EDT)June 3,10,17,24;July 1,8,15,22,29;August 5,12,19,26"]
    False
  , RecurTest "Every Friday the 13th, forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["EXDATE;TZID=US-Eastern:19970902T090000\nRRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13"])
    ["(1998 9:00 AM EST)February 13;March 13;November 13"
    , "(1999 9:00 AM EDT)August 13"
    , "(2000 9:00 AM EDT)October 13"]
    False
  , RecurTest "The first Saturday that follows the first Sunday of the month, forever:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970913T090000"
      ["RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13"])
    ["(1997 9:00 AM EDT)September 13;October 11"
    , "(1997 9:00 AM EST)November 8;December 13"
    , "(1998 9:00 AM EST)January 10;February 7;March 7"
    , "(1998 9:00 AM EDT)April 11;May 9;June 13"]
    False
  , RecurTest "Every four years, the first Tuesday after a Monday in November, forever (U.S. Presidential Election day):"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19961105T090000"
      ["RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8"])
    ["(1996 9:00 AM EST)November 5"
    , "(2000 9:00 AM EST)November 7"
    , "(2004 9:00 AM EST)November 2"]
    False
  , RecurTest "The 3rd instance into the month of one of Tuesday, Wednesday or Thursday, for the next 3 months:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970904T090000"
      ["RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3"])
    ["(1997 9:00 AM EDT)September 4;October 7"
    , "(1997 9:00 AM EST)November 6"]
    True
  , RecurTest "The 2nd to last weekday of the month:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970929T090000"
      ["RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2"])
    ["(1997 9:00 AM EDT)September 29"
    , "(1997 9:00 AM EST)October 30;November 27;December 30"
    , "(1998 9:00 AM EST)January 29;February 26;March 30"]
    False
  , RecurTest "Every 3 hours from 9:00 AM to 5:00 PM on a specific day:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z"])
    ["(September 2, 1997 EDT)09:00,12:00,15:00"]
    True
  , RecurTest "Every 15 minutes for 6 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6"])
    ["(September 2, 1997 EDT)09:00,09:15,09:30,09:45,10:00,10:15"]
    True
  , RecurTest "Every hour and a half for 4 occurrences:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4"])
    ["(September 2, 1997 EDT)09:00,10:30,12:00,13:30"]
    True
  , RecurTest "Every 20 minutes from 9:00 AM to 4:40 PM every day:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970902T090000"
      ["RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40", "RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16"])
    ["(September 2, 1997 EDT)09:00,09:20,09:40,10:00,10:20,10:40,11:00,11:20,11:40,12:00,12:20,12:40,13:00,13:20,13:40,14:00,14:20,14:40,15:00,15:20,15:40,16:00,16:20,16:40"
    , "(September 3, 1997 EDT)09:00,09:20,09:40,10:00,10:20,10:40,11:00,11:20,11:40,12:00,12:20,12:40,13:00,13:20,13:40,14:00,14:20,14:40,15:00,15:20,15:40,16:00,16:20,16:40"]
    False
  , RecurTest "An example where the days generated makes a difference because of WKST:"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970805T090000"
      ["RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO"])
    ["(1997 9:00 AM EDT)August 5,10,19,24"]
    True
  , RecurTest "changing only WKST from MO to SU, yields different results"
    (RRuleTestData "DTSTART;TZID=US-Eastern:19970805T090000"
      ["RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU"])
    ["(1997 9:00 AM EDT)August 5,17,19,31"]
    True
  , RecurTest "An example where an invalid date (i.e., February 30) is ignored."
    (RRuleTestData "DTSTART;TZID=US-Eastern:20070115T090000"
      ["RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5"])
    [ "(2007 9:00 AM EST)January 15,30"
    , "(2007 9:00 AM EST)February 15"
    , "(2007 9:00 AM EST)March 15,30"]
    True
  ]
