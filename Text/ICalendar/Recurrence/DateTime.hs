module Text.ICalendar.Recurrence.DateTime
  (
    vUTCTimes
  ) where
import           Control.Monad                   (join)
import           Data.Time
import           Text.ICalendar.Recurrence.Types
import           Text.ICalendar.Types


vUTCTimes :: HasRRule r => TimeToOffset -> TZIDToOffset -> r
                            -> Either ICalError (Maybe UTCTime, Maybe UTCTime)
vUTCTimes ftz ztz r = do
    let
      toUtc = vDateTimeToUtc ftz ztz
      end :: Maybe UTCTime -> Maybe (Either DTEnd DurationProp) -> Either ICalError (Maybe UTCTime)
      end _ Nothing = Right Nothing
      end _ (Just (Left (DTEnd d _))) = sequence $ Just (toUtc d)
      end s (Just (Right (DurationProp dp _))) = Right $ addUTCTime (durationSeconds dp) <$> s
    startTime <- sequence $ toUtc . dtStartValue <$> vDTStart r
    endTime <- end startTime (vDTEndDuration r)
    pure (startTime, endTime)

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

vDateTimeToUtc :: TimeToOffset -> TZIDToOffset -> VDateTime -> Either ICalError UTCTime
vDateTimeToUtc ftz ztz (VDateTime dt) = dateTimeToUtc ftz ztz dt
vDateTimeToUtc ftz _   (VDate d)      = dateToUtc ftz d

dateTimeToUtc :: TimeToOffset -> TZIDToOffset -> DateTime -> Either ICalError UTCTime
dateTimeToUtc ftz _  (FloatingDateTime d) = flip localTimeToUTC d <$> ftz d
dateTimeToUtc _   _  (UTCDateTime d)      = Right d
dateTimeToUtc _  ztz (ZonedDateTime d tz) = flip localTimeToUTC d <$> ztz tz d

dateToUtc :: TimeToOffset -> Date -> Either ICalError UTCTime
dateToUtc ftz (Date d) = flip localTimeToUTC time <$> ftz time
  where time = LocalTime d midnight
