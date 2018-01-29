-- | Component Properties
--
-- <https://tools.ietf.org/html/rfc5545#section-3.8>
module Text.ICalendar.Properties
    ( ICalendarProperty(..)
    , PropertyParser
    , PropertyPrinter

    , module Text.ICalendar.Properties.Alarm

    {-
    , module Text.ICalendar.Properties.Calendar
    , module Text.ICalendar.Properties.ChangeManagement
    , module Text.ICalendar.Properties.DateTime
    , module Text.ICalendar.Properties.Descriptive
    , module Text.ICalendar.Properties.Misc
    , module Text.ICalendar.Properties.Recurrence
    , module Text.ICalendar.Properties.Relationship
    , module Text.ICalendar.Properties.TimeZone
    -}
    , parseOtherParameters
    , optParam1
    , reqParam1
    , defParam1
    ) where

import           Text.ICalendar.Properties.Alarm
import           Text.ICalendar.Properties.Internal
