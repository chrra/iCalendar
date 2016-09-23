module Text.ICalendar.Recurrence.DateTime
  (
    vUTCTimes
  ) where
import           Control.Monad                   (join)
import           Data.Time
import           Text.ICalendar.Recurrence.Types
import           Text.ICalendar.Types


vUTCTimes :: HasRRule r => TimeToOffset -> TZIDToOffset -> r
                            -> (Maybe UTCTime, Maybe UTCTime)
vUTCTimes ftz ztz r = (startTime, endTime)
  where
    toUtc = vDateTimeToUtc ftz ztz
    startTime = join $ toUtc . dtStartValue <$> vDTStart r
    endTime = join $ end <$> vDTEndDuration r
    end :: Either DTEnd DurationProp -> Maybe UTCTime
    end (Left (DTEnd d _)) = toUtc d
    end (Right (DurationProp dp _)) =
      addUTCTime (durationSeconds dp) <$> startTime

durationSeconds :: Num a => Duration -> a
durationSeconds = ds
  where
    ds (DurationDate sign day hour minute second) =
      signNum sign (dayL day + hourL hour + minuteL minute +  fromIntegral second)
    ds (DurationTime sign hour minute second) =
      signNum sign (hourL hour + minuteL minute + fromIntegral second)
    ds (DurationWeek sign week) = signNum sign (weekL week)
    weekL = (*) (dayL 7) . fromIntegral
    dayL = (*) (hourL 24) . fromIntegral
    hourL = (*) (minuteL 60) . fromIntegral
    minuteL = (*) 60 . fromIntegral
    signNum Positive = id
    signNum Negative = negate

vDateTimeToUtc :: TimeToOffset -> TZIDToOffset -> VDateTime -> Maybe UTCTime
vDateTimeToUtc ftz ztz (VDateTime dt) = dateTimeToUtc ftz ztz dt
vDateTimeToUtc ftz _   (VDate d)      = dateToUtc ftz d

dateTimeToUtc :: TimeToOffset -> TZIDToOffset -> DateTime -> Maybe UTCTime
dateTimeToUtc ftz _  (FloatingDateTime d) = flip localTimeToUTC d <$> ftz d
dateTimeToUtc _   _  (UTCDateTime d)      = Just d
dateTimeToUtc _  ztz (ZonedDateTime d tz) = flip localTimeToUTC d <$> ztz tz d

dateToUtc :: TimeToOffset -> Date -> Maybe UTCTime
dateToUtc ftz (Date d) = flip localTimeToUTC time <$> ftz time
  where time = LocalTime d midnight
