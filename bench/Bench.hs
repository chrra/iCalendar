{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import           Codec.MIME.Type               (MIMEType (..), Multipart (..))
import           Control.DeepSeq               (NFData)
import           Criterion.Main
import           Data.ByteString.Lazy          (ByteString)
import           Data.Default
import           GHC.Generics                  (Generic)

import           Text.ICalendar
import           Text.ICalendar.Parser.Content

deriving instance Generic MIMEType
instance NFData MIMEType
deriving instance Generic Multipart
instance NFData Multipart

instance NFData Language
instance NFData OtherParam
instance NFData OtherParams
instance NFData VCalendar
instance NFData ProdId
instance NFData ICalVersion
instance NFData Scale
instance NFData Method
instance NFData VEvent
instance NFData VTodo
instance NFData VJournal
instance NFData VFreeBusy
instance NFData VTimeZone
instance NFData TZProp
instance NFData VAlarm
instance NFData VOther
instance NFData Attach
instance NFData Categories
instance NFData Class
instance NFData ClassValue
instance NFData Completed
instance NFData Comment
instance NFData Description
instance NFData Geo
instance NFData Location
instance NFData PercentComplete
instance NFData Priority
instance NFData Resources
instance NFData EventStatus
instance NFData TodoStatus
instance NFData JournalStatus
instance NFData Summary
instance NFData Date
instance NFData DateTime
instance NFData DTEnd
instance NFData Due
instance NFData DTStart
instance NFData Duration
instance NFData Sign
instance NFData DurationProp
instance NFData FreeBusy
instance NFData Period
instance NFData UTCPeriod
instance NFData FBType
instance NFData Transp
instance NFData TZID
instance NFData TZName
instance NFData UTCOffset
instance NFData TZUrl
instance NFData Attendee
instance NFData CUType
instance NFData Role
instance NFData PartStat
instance NFData Contact
instance NFData Organizer
instance NFData RecurrenceId
instance NFData Range
instance NFData RelatedTo
instance NFData RelType
instance NFData URL
instance NFData UID
instance NFData ExDate
instance NFData RDate
instance NFData Freq
instance NFData Weekday
instance NFData Recur
instance NFData RRule
instance NFData Repeat
instance NFData Related
instance NFData Trigger
instance NFData Created
instance NFData DTStamp
instance NFData LastModified
instance NFData Sequence
instance NFData RequestStatus
instance NFData OtherProperty


p :: ByteString -> [Content]
p = either error id . parseToContent def

p' :: ByteString -> ([VCalendar], [String])
p' = either error id . parseICalendar def


line :: ByteString
line = "A;X=Y;Z=B:AA\r\n\tAAA\r\n"

cal :: ByteString
cal = "\
\BEGIN:VCALENDAR\r\n\
\PRODID:-//xyz Corp//NONSGML PDA Calendar Version 1.0//EN\r\n\
\VERSION:2.0\r\n\
\BEGIN:VEVENT\r\n\
\DTSTAMP:19960704T120000Z\r\n\
\UID:uid1@example.com\r\n\
\ORGANIZER:mailto:jsmith@example.com\r\n\
\DTSTART:19960918T143000Z\r\n\
\DTEND:19960920T220000Z\r\n\
\STATUS:CONFIRMED\r\n\
\CATEGORIES:CONFERENCE\r\n\
\SUMMARY:Networld+Interop Conference\r\n\
\DESCRIPTION:Networld+Interop Conference\r\n\
\ and Exhibit\\nAtlanta World Congress Center\\n\r\n\
\ Atlanta\\, Georgia\r\n\
\END:VEVENT\r\n\
\END:VCALENDAR\n"

main :: IO ()
main = defaultMain
    [ bgroup "content parser"
        [ bench "parse line" $ nf p line
        , bench "parse example" $ nf p cal
        ]
    , bgroup "full parse"
        [ bench "parse" $ nf p' cal
        ]
    ]
