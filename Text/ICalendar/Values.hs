{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Alarm Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8.6>
module Text.ICalendar.Values
    (
      ValuePrinter
    , ValueParser
    , ICalendarValue
    -- | Omitted:
    -- @
    --     Boolean = 'Bool'
    -- @
    , Value(..)
    , Binary(..)
    , CalAddress(..)
    , Date(..)
    , DateTime(..)
    , Duration(..)
    , Sign(..)
    -- | Omitted:
    --
    -- @
    --     Float = 'Float'
    --     Integer = 'Int'
    -- @
    , Period(..)
    , UTCPeriod(..)
    -- * Recurrence rules
    , Recur(..)
    , Freq(..)
    , Weekday(..)
    -- | Omitted:
    --
    -- @
    --     Text = 'Text'
    --     Time = ???
    --     URI = 'URI'
    -- @
    , UTCOffset(..)
    -- * Values that are really Text but that can be refined further:
    , ClassValue(..)
    ) where

import           Control.Monad                  (unless)
import qualified Data.ByteString.Base64.Lazy    as B64
import           Data.ByteString.Lazy.Char8     (ByteString)
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.CaseInsensitive           (CI)
import qualified Data.CaseInsensitive           as CI
import           Data.Default                   (Default (..))
import           Data.Foldable                  (fold)
import           Data.List                      (intercalate)
import           Data.Maybe                     (fromJust, isJust, isNothing)
import           Data.Text.Lazy                 (Text)
import           Data.Time                      (Day, LocalTime, UTCTime)
import qualified Data.Time                      as T
import           Data.Traversable               (forM)
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)
import           Network.URI                    (URI)
import qualified Network.URI                    as URI

import           Text.ICalendar.Internal
import           Text.ICalendar.Parameters
import           Text.ICalendar.Values.Internal


vp :: ByteString -> ValuePrinter
vp x = ValuePrinter ($ x)

checkValueType :: forall a. ICalendarValue a => a -> ValueParser ()
checkValueType _ = do
    typ <- optParam1
    unless (isNothing typ || typ == Just (valueType (undefined :: a))) $
        pErr $ "checkValueType: got VALUE=" ++ show typ

lastToMaybe :: [a] -> Maybe a
lastToMaybe x = if null x then Nothing else Just $ last x

-- | Value, 3.2.20
data Value = ValueBinary Binary
           | ValueBoolean Bool
           | ValueCalAddress CalAddress
           | ValueDate Date
           | ValueDateTime DateTime
           | ValueDuration Duration
           | ValueFloat Float
           | ValueInteger Int
           | ValuePeriod Period
           | ValueRecur Recur
           | ValueText Text
           | ValueTime
           | ValueUri URI
           | ValueUtcOffset
           | ValueX (CI Text) Text
             deriving (Show, Eq, Ord, Typeable, Generic)

-- | Binary value, 3.3.1
data Binary
    = Binary
    { binaryFMTType :: Maybe FMTType
    , binaryValue   :: ByteString
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarValue Binary where
    valueType _ =
        ValueTypeBinary

    valueParser = do
        checkValueType (undefined :: Binary)

        enc <- defParam1
        mmime <- optParam1
        unless (enc == Base64) $
            pErr "valueParser{Binary}: encoding not base64"
        val <- getValue
        case B64.decode val of
            Left e -> pErr $ "valueParser{Binary}: bad base64, " ++ e
            Right v -> return $ Binary mmime v

    valuePrinter Binary {..} =
        vp $ B64.encode binaryValue

    valueParameterPrinter Binary {..} =
        fold $ parameterPrinter <$> binaryFMTType

-- Boolean value, 3.3.2

instance ICalendarValue Bool where
    valueType _ = ValueTypeBoolean

    valueParser = do
        checkValueType True
        v <- CI.mk <$> getValue
        case v of
            "TRUE" -> return True
            "FALSE" -> return False
            x -> pErr $ "valueParser{Bool}: not \"TRUE\" or \"FALSE\": " ++
                        show x

    valuePrinter True = vp "TRUE"
    valuePrinter False = vp "FALSE"

    valueParameterPrinter _ = mempty


-- | Calendar User Address, 3.3.3.
data CalAddress
    = CalAddress
    { calAddressValue :: URI
      -- TODO: move all parameters that can be on a CalAddress into this from
      -- the property.
    } deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarValue CalAddress where
    valueType _ = ValueTypeCalAddress

    valueParser = do
        checkValueType (undefined :: CalAddress)
        val <- getValue
        case URI.parseURI (B.unpack val) of
            Just x  -> return $ CalAddress x
            Nothing -> pErr "valueParser{CalAddress}: invalid URI"

    valuePrinter = vp . B.pack . show . calAddressValue

    valueParameterPrinter _ = mempty

-- | Date, 3.3.4.
newtype Date
    = Date
    { dateValue :: Day
    } deriving (Show, Eq, Ord, Typeable, Generic)

parseDate :: String -> ValueParser Date
parseDate v = case T.parseTimeM True T.defaultTimeLocale "%Y%m%d" v of
    Just d  -> return $ Date d
    Nothing -> pErr $ "valueParser{Date}: invalid date: " ++ v

instance ICalendarValue [Date] where
    valueType _ = ValueTypeDate

    valueParser = do
        checkValueType (undefined :: Date)
        vs <- map B.unpack . B.split ',' <$> getValue
        forM vs parseDate

    valuePrinter =
        vp . B.pack . intercalate ","
           . map (T.formatTime T.defaultTimeLocale "%Y%m%d" . dateValue)

    valueParameterPrinter = mempty

instance ICalendarValue Date where
    valueType _ = ValueTypeDate

    valueParser = do
        checkValueType (undefined :: Date)
        parseDate =<< B.unpack <$> getValue

    valuePrinter =
        vp . B.pack . T.formatTime T.defaultTimeLocale "%Y%m%d" . dateValue

    valueParameterPrinter _ = mempty


-- | Date-Time, 3.3.5.
data DateTime
    = FloatingDateTime
    { dateTimeFloating :: LocalTime
    }
    | UTCDateTime
    { dateTimeUTC :: UTCTime
    }
    | ZonedDateTime
    { dateTimeFloating :: LocalTime
    , dateTimeZone     :: TZID
    } deriving (Show, Eq, Ord, Typeable, Generic)

parseDateTime :: String -> ValueParser (LocalTime, Bool)
parseDateTime v =
    case lastToMaybe $ T.readSTime True T.defaultTimeLocale "%Y%m%dT%H%M%S" v of
        Just (t, "")  -> return (t, False)
        Just (t, "Z") -> return (t, True)
        Just (_, xs)  -> pErr $ "valueParser{DateTime}: trailing: " ++ xs
        Nothing       -> pErr $ "valueParser{DateTime}: invalid dateTime: " ++ v

parseDateTime' :: Maybe TZID -> String -> ValueParser DateTime
parseDateTime' mid s = do
    (x@(T.LocalTime day tod), z) <- parseDateTime s
    if | z && isJust mid ->
        pErr "valueParser{DateTime}: UTC DateTime with TZID"
       | z ->
        return . UTCDateTime . T.UTCTime day $ T.timeOfDayToTime tod
       | isJust mid ->
        return $ ZonedDateTime x (fromJust mid)
       | otherwise ->
        return $ FloatingDateTime x

printDateTime' :: DateTime -> String
printDateTime' (FloatingDateTime x) = printDateTime x
printDateTime' (UTCDateTime x)      = printDateTime x ++ "Z"
printDateTime' (ZonedDateTime x _)  = printDateTime x

instance ICalendarValue [DateTime] where
    valueType _ = ValueTypeDateTime

    valueParser = do
        checkValueType (undefined :: DateTime)
        mtzid <- optParam1
        mapM (parseDateTime' mtzid . B.unpack) . B.split ',' =<< getValue

    valuePrinter = vp . B.pack . intercalate "," . map printDateTime'

    valueParameterPrinter (ZonedDateTime {..}:_) = parameterPrinter dateTimeZone
    valueParameterPrinter _ = mempty

instance ICalendarValue DateTime where
    valueType _ = ValueTypeDateTime
    valueParser = do
        checkValueType (undefined :: DateTime)
        mtzid <- optParam1
        parseDateTime' mtzid . B.unpack =<< getValue

    valuePrinter = vp . B.pack . printDateTime'

    valueParameterPrinter ZonedDateTime {..} = parameterPrinter dateTimeZone
    valueParameterPrinter _ = mempty

-- | Duration, 3.3.6.
data Duration -- TODO(?): Convert to DiffTime? Fixes 'DurationProp' conflict.
    = DurationDate
    { durSign   :: Sign -- ^ 'def' = 'Positive'
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
    { durSign :: Sign
    , durWeek :: Int
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Sign, in duration, 3.3.10.
data Sign = Positive | Negative
            deriving (Show, Eq, Ord, Typeable, Generic)

-- | 'Positive'.
instance Default Sign where
    def = Positive

-- | Period of time, 3.3.9.
data Period
    = PeriodDates    DateTime DateTime
    | PeriodDuration DateTime Duration
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Period of time which must be UTC, as in FreeBusy, 3.3.9.
data UTCPeriod
    = UTCPeriodDates    UTCTime UTCTime
    | UTCPeriodDuration UTCTime Duration
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Recur, 3.3.10.
data Recur = Recur
    { recurFreq       :: Freq
    , recurUntilCount :: Maybe (Either (Either Date DateTime) Int)
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
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Frequency, in recurrences, 3.3.10.
data Freq
    = Secondly
    | Minutely
    | Hourly
    | Daily
    | Weekly
    | Monthly
    | Yearly
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Weekday, in recurrences, 3.3.10.
data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday
                      | Friday | Saturday
               deriving (Show, Eq, Ord, Bounded, Enum, Typeable, Generic)

-- | UTC Offset, 3.3.14
--
-- TODO: clean, don't use as TZOffsetTo/TZOffsetFrom
data UTCOffset = UTCOffset
    { utcOffsetValue :: Int -- ^ Number of seconds away from UTC
    , utcOffsetOther :: OtherParameters
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Classification value. 3.8.1.3.
--
-- Unrecognized ClassValueX MUST be treated as Private.
data ClassValue
    = Public
    | Private
    | Confidential
    | ClassValueX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | 'Public'.
instance Default ClassValue where
    def = Public
