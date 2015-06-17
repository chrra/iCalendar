{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Text.ICalendar.Parser.Properties where

import           Control.Applicative
import           Control.Monad.Error              hiding (mapM)
import           Control.Monad.RWS                (asks)
import qualified Data.ByteString.Base64.Lazy      as B64
import qualified Data.ByteString.Lazy.Char8       as B
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI
import           Data.Char
import           Data.Default
import           Data.Maybe
import qualified Data.Set                         as S
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as T
import           Data.Traversable                 (mapM)
import qualified Data.Version                     as Ver
import           Prelude                          hiding (mapM)
import           Text.ParserCombinators.ReadP     (readP_to_S)

import           Text.Parsec.Prim                 hiding ((<|>))

import           Text.ICalendar.Parser.Common
import           Text.ICalendar.Parser.Parameters
import           Text.ICalendar.Types

parseFreeBusy :: Content -> ContentParser FreeBusy
parseFreeBusy (ContentLine _ "FREEBUSY" o bs) = do
    typ <- maybe (return def) (parseFBType . CI.mk .: paramOnlyOne) $
                     lookup "FBTYPE" o
    periods <- S.fromList .: mapM parseUTCPeriod $ B.split ',' bs
    return $ FreeBusy typ periods (toO $ filter ((/="FBTYPE").fst) o)
parseFreeBusy x = throwError $ "parseFreeBusy: " ++ show x

parseXDurationOpt :: CI Text
                  -> (DateTime -> OtherParams -> a)
                  -> (Date     -> OtherParams -> a)
                  -> Maybe DTStart
                  -> ContentParser (Maybe (Either a DurationProp))
parseXDurationOpt w a b dts = do
    dte <- optLine1 w $ Just .: parseSimpleDateOrDateTime a b
    dur <- optLine1 "DURATION" $ Just .: parseDurationProp dts
    case (dte, dur) of
         (Nothing, Nothing) -> return Nothing
         (Just x, Nothing) -> return . Just $ Left x
         (Nothing, Just x) -> return . Just $ Right x
         _ -> throwError "Either DTEND or DURATION can be specified, but not \
                         \both."

-- | Parse trigger. 3.8.6.3
parseTrigger :: Content -> ContentParser Trigger
parseTrigger (ContentLine _ "TRIGGER" o bs) = do
    value <- paramOnlyOne . fromMaybe ["DURATION"] $ lookup "VALUE" o
    case value of
         "DURATION" -> do
            rel <- maybe (return def)
                         (parseAlarmTriggerRelationship . CI.mk
                               <=< paramOnlyOne) $ lookup "RELATED" o
            let o' = filter (\(x,_) -> x /= "VALUE" && x /= "RELATED") o
            val <- parseDuration "TRIGGER" bs
            return $ TriggerDuration val rel (toO o')
         "DATE-TIME" -> do val <- mustBeUTC =<< parseDateTime Nothing bs
                           let o' = filter (\(x, _) -> x /= "VALUE") o
                           return $ TriggerDateTime val (toO o')
         x -> throwError $ "parseTrigger: invalid value: " ++ show x
parseTrigger x = throwError $ "parseTrigger: " ++ show x


-- | Parse related to. 3.8.4.5
parseRelatedTo :: Content -> ContentParser RelatedTo
parseRelatedTo (ContentLine _ "RELATED-TO" o bs) = do
    val <- valueOnlyOne =<< parseText bs
    typ <- maybe (return def) (parseRelationshipType . CI.mk .: paramOnlyOne) $
                lookup "RELTYPE" o
    return $ RelatedTo val typ (toO $ filter (\(x,_) -> x /= "RELTYPE") o)
parseRelatedTo x = throwError $ "parseRelatedTo: " ++ show x


-- | Parse request status. 3.8.8.3
parseRequestStatus :: Content -> ContentParser RequestStatus
parseRequestStatus (ContentLine _ "REQUEST-STATUS" o bs) = do
    let (statcode', rest) = B.break (==';') bs
        statcode :: Maybe [Int]
        statcode = mapM (maybeRead . B.unpack) $ B.split '.' statcode'
    when (isNothing statcode) .
        throwError $ "parseRequestStatus: invalid code: " ++ show bs
    when (B.null rest) .
        throwError $ "parseRequestStatus: missing statdesc: " ++ show bs
    (statdesc, rest') <- (\(a,b) -> (,b) <$> valueOnlyOne a)
                                   <=< parseText' $ B.tail rest
    statext <- if B.null rest'
                  then return Nothing
                  else do when (B.head rest' /= ';') .
                              throwError $ "parseRequestStatus: bad desc: " ++
                                               show bs
                          Just <$> (valueOnlyOne =<< parseText (B.tail rest'))
    lang <- mapM paramOnlyOne $ Language . CI.mk .: lookup "LANGUAGE" o
    let o' = filter (\(x, _) -> x `notElem` ["LANGUAGE"]) o
    return $ RequestStatus (fromJust statcode) statdesc lang statext (toO o')
parseRequestStatus x = throwError $ "parseRequestStatus: " ++ show x

-- | Parse exception date-times. 3.8.5.1
parseExDate :: Content -> ContentParser ExDate
parseExDate (ContentLine _ "EXDATE" o bs) = do
    (typ, tzid, o') <- typTzIdO o
    let bs' = B.split ',' bs
    case typ of
         "DATE-TIME" -> do xs <- mapM (parseDateTime tzid) bs'
                           return . ExDateTimes (S.fromList xs) $ toO o'
         "DATE" -> do xs <- mapM parseDate bs'
                      return . ExDates (S.fromList xs) $ toO o'
         _ -> throwError $ "Invalid type: " ++ show typ
parseExDate x = throwError $ "parseExDate: " ++ show x

-- | Parse categories. 3.8.1.2
parseCategories :: Content -> ContentParser Categories
parseCategories (ContentLine _ "CATEGORIES" o bs) = do
    vals <- parseText bs
    lang <- mapM paramOnlyOne $ Language . CI.mk .: lookup "LANGUAGE" o
    let o' = filter (\(x, _) -> x `notElem` ["LANGUAGE"]) o
    return $ Categories (S.fromList vals) lang (toO o')
parseCategories x = throwError $ "parseCategories: " ++ show x

-- | Parse attendee. 3.8.4.1
parseAttendee :: Content -> ContentParser Attendee
parseAttendee (ContentLine _ "ATTENDEE" o bs) = do
    attendeeValue <- parseURI =<< asks (T.unpack . ($ bs) . dfBS2Text)
    attendeeCUType <- g (parseCUType . CI.mk .: paramOnlyOne) $
                        lookup "CUTYPE" o
    attendeeMember <- g (S.fromList .: mapM (parseURI . T.unpack)) $
                        lookup "MEMBER" o
    attendeeRole <- g (parseRole . CI.mk .: paramOnlyOne) $ lookup "ROLE" o
    attendeePartStat <- g (parsePartStat . CI.mk .: paramOnlyOne) $
                        lookup "PARTSTAT" o
    attendeeRSVP <- maybe (return False) (parseBool . CI.mk <=< paramOnlyOne) $
                        lookup "RSVP" o
    attendeeDelTo <- g (S.fromList .: mapM (parseURI . T.unpack)) $
                        lookup "DELEGATED-TO" o
    attendeeDelFrom <- g (S.fromList .: mapM (parseURI . T.unpack)) $
                        lookup "DELEGATED-FROM" o
    attendeeSentBy <- mapM (parseURI . T.unpack <=< paramOnlyOne) $
                        lookup "SENT-BY" o
    attendeeCN <- mapM paramOnlyOne $ lookup "CN" o
    attendeeDir <- mapM (parseURI . T.unpack <=< paramOnlyOne) $
                        lookup "DIR" o
    attendeeLanguage <- mapM (Language . CI.mk .: paramOnlyOne) $
                        lookup "LANGUAGE" o
    let attendeeOther = toO $ filter f o
        f (x, _) = x `notElem` [ "CUTYPE", "MEMBER", "ROLE", "PARTSTAT", "RSVP"
                               , "DELEGATED-TO", "DELEGATED-FROM", "SENT-BY"
                               , "CN", "DIR"]
    return Attendee {..}
  where g :: (Monad m, Default b) => (a -> m b) -> Maybe a -> m b
        g = maybe $ return def
parseAttendee x = throwError $ "parseAttendee: " ++ show x


-- | Parse attachment. 3.8.1.1
parseAttachment :: Content -> ContentParser Attachment
parseAttachment (ContentLine _ "ATTACH" o bs) = do
    fmt <- mapM (parseMime <=< paramOnlyOne) $ lookup "FMTTYPE" o
    val <- mapM paramOnlyOne $ lookup "VALUE" o
    case val of
         Just "BINARY" -> do
             enc <- mapM paramOnlyOne $ lookup "ENCODING" o
             case enc of
                  Just "BASE64" ->
                      case B64.decode bs of
                           Left e -> throwError $ "parseAttachment: invalid \
                                                  \base64: " ++ e
                           Right v -> return $ BinaryAttachment fmt v
                                                  (toO $ filter binF o)
                  _ -> throwError $ "parseAttachment: invalid encoding: " ++
                                        show enc
         Nothing -> do uri <- parseURI =<< asks (T.unpack . ($ bs) . dfBS2Text)
                       return $ UriAttachment fmt uri (toO $ filter f o)
         _ -> throwError $ "parseAttachment: invalid value: " ++ show val
  where binF a@(x, _) = f a && x /= "VALUE" && x /= "ENCODING"
        f (x, _) = x /= "FMTTYPE"
parseAttachment x = throwError $ "parseAttachment: " ++ show x

parseDurationProp :: Maybe DTStart -> Content -> ContentParser DurationProp
parseDurationProp dts (ContentLine _ "DURATION" o bs) = do
    val <- parseDuration "DURATION" bs
    case (dts, val) of
         (Just DTStartDate {}, DurationDate {..})
            | durHour == 0 && durMinute == 0 && durSecond == 0 -> return ()
         (Just DTStartDate {}, DurationWeek {}) -> return ()
         (Just DTStartDate {}, _) ->
             throwError "DURATION must be in weeks or days when DTSTART \
                        \has VALUE DATE and not DATE-TIME."
         _ -> return ()
    return . DurationProp val $ toO o
parseDurationProp _ x = throwError $ "parseDurationProp: " ++ show x

parseRecurId :: Maybe DTStart -> Content -> ContentParser RecurrenceId
parseRecurId dts (ContentLine p "RECURRENCE-ID" o bs) = do
    range' <- mapM (parseRange . CI.mk <=< paramOnlyOne) $ lookup "RANGE" o
    recurid <- parseSimpleDateOrDateTime
            (($ range') . RecurrenceIdDateTime)
            (($ range') . RecurrenceIdDate)
            (ContentLine p "RECURRENCE-ID" (filter ((/="RANGE").fst) o) bs)
    case (dts, recurid) of
         (Nothing, _) -> return recurid
         (Just DTStartDate {}, RecurrenceIdDate {}) ->
             return recurid
         (Just DTStartDateTime {dtStartDateTimeValue = v},
          RecurrenceIdDateTime {recurrenceIdDateTime = r}) ->
               case (v, r) of -- TODO: Check this. iff confuse me.
                    (UTCDateTime {}, FloatingDateTime {}) -> err dts recurid
                    (UTCDateTime {}, ZonedDateTime {})    -> err dts recurid
                    (FloatingDateTime {}, UTCDateTime {}) -> err dts recurid
                    (ZonedDateTime {}, UTCDateTime {})    -> err dts recurid
                    _ -> return recurid
         _ -> err dts recurid
  where err d r = throwError $ "parseRecurId: DTSTART local time mismatch: " ++
                                show (d, r)
parseRecurId _ x = throwError $ "parseRecurId: " ++ show x

-- | Parse time transparency. 3.8.2.7
parseTransp :: Content -> ContentParser TimeTransparency
parseTransp (ContentLine _ "TRANSP" o x)
    | CI.mk x == "OPAQUE" = return . Opaque $ toO o
    | CI.mk x == "TRANSPARENT" = return . Transparent $ toO o
parseTransp x = throwError $ "parseTransp: " ++ show x

-- | Parse event status. 3.8.1.11
parseEventStatus :: Content -> ContentParser EventStatus
parseEventStatus (ContentLine _ "STATUS" o x)
    | CI.mk x == "TENTATIVE" = return . TentativeEvent $ toO o
    | CI.mk x == "CONFIRMED" = return . ConfirmedEvent $ toO o
    | CI.mk x == "CANCELLED" = return . CancelledEvent $ toO o
parseEventStatus x = throwError $ "parseEventStatus: " ++ show x

-- | Parse todo status. 3.8.1.11
parseTodoStatus :: Content -> ContentParser TodoStatus
parseTodoStatus (ContentLine _ "STATUS" o x)
    | CI.mk x == "NEEDS-ACTION" = return . TodoNeedsAction $ toO o
    | CI.mk x == "COMPLETED"    = return . CompletedTodo   $ toO o
    | CI.mk x == "IN-PROCESS"   = return . InProcessTodo   $ toO o
    | CI.mk x == "CANCELLED"    = return . CancelledTodo   $ toO o
parseTodoStatus x = throwError $ "parseTodoStatus: " ++ show x

-- | Parse journal status. 3.8.1.11
parseJournalStatus :: Content -> ContentParser JournalStatus
parseJournalStatus (ContentLine _ "STATUS" o x)
    | CI.mk x == "DRAFT"     = return . DraftJournal     $ toO o
    | CI.mk x == "FINAL"     = return . FinalJournal     $ toO o
    | CI.mk x == "CANCELLED" = return . CancelledJournal $ toO o
parseJournalStatus x = throwError $ "parseJournalStatus: " ++ show x

-- | Parse organizer. 3.8.4.3
parseOrganizer :: Content -> ContentParser Organizer
parseOrganizer (ContentLine _ "ORGANIZER" o bs) = do
    organizerValue <- parseURI =<< asks (T.unpack . ($ bs) . dfBS2Text)
    organizerCN <- mapM paramOnlyOne $ lookup "CN" o
    organizerDir <- mapM (parseURI . T.unpack <=< paramOnlyOne) $ lookup "DIR" o
    organizerSentBy <- mapM (parseURI . T.unpack <=< paramOnlyOne) $
        lookup "SENT-BY" o
    organizerLanguage <- mapM (Language . CI.mk .: paramOnlyOne) $
        lookup "LANGUAGE" o
    let f x = x `notElem` ["CN", "DIR", "SENT-BY", "LANGUAGE"]
        o' = filter (f . fst) o
    return Organizer { organizerOther = toO o', .. }
parseOrganizer x = throwError $ "parseOrganizer: " ++ show x

-- | Parse geographic position. 3.8.1.6
parseGeo :: Content -> ContentParser Geo
parseGeo (ContentLine _ "GEO" o bs) = do
    let (lat', long') = B.break (==';') bs
        lat = maybeRead . stripPlus $ B.unpack lat' :: Maybe Float
        long = maybeRead . stripPlus . B.unpack $ B.tail long' :: Maybe Float
    when (B.null long' || isNothing (lat >> long)) .
        throwError $ "Invalid latitude/longitude: " ++ show bs
    return $ Geo (fromJust lat) (fromJust long) (toO o)
  where stripPlus ('+':xs) = xs
        stripPlus xs = xs
parseGeo x = throwError $ "parseGeo: " ++ show x

-- | Parse classification. 3.8.1.3
parseClass :: Content -> ContentParser Class
parseClass (ContentLine _ "CLASS" o bs) = do
    iconv <- asks dfBS2IText
    return . flip Class (toO o) $
        case iconv bs of
             "PUBLIC" -> Public
             "PRIVATE" -> Private
             "CONFIDENTIAL" -> Confidential
             x -> ClassValueX x
parseClass x = throwError $ "parseClass: " ++ show x

-- | Parse TZName. 3.8.3.1
parseTZName :: Content -> ContentParser TZName
parseTZName (ContentLine _ "TZNAME" o bs) = do
    txt <- valueOnlyOne =<< parseText bs
    lang <- mapM paramOnlyOne $ Language . CI.mk .: lookup "LANGUAGE" o
    return $ TZName txt lang (toO o)
parseTZName x = throwError $ "parseTZName: " ++ show x


-- | Parse a VERSION property 3.7.4
parseVersion :: Content -> ContentParser ICalVersion
parseVersion (ContentLine _ "VERSION" o bs) = do
    c <- asks dfBS2Text
    let (maxver', minver'') = break (==';') . T.unpack $ c bs
        minver' = drop 1 minver''
        parseVer = fst .: listToMaybe . filter ((=="") . snd)
                                       . readP_to_S Ver.parseVersion
        maxver = parseVer maxver'
        minver = parseVer minver'
        [maxJ, minJ] = fromJust <$> [maxver, minver]
    when (isNothing maxver) .
        throwError $ "parseVersion: error parsing version: " ++ show maxver'
    if null minver''
       then return $ MaxICalVersion maxJ (toO o)
       else do when (isNothing minver) .
                    throwError $ "parseVersion: error parsing version: "
                                   ++ show minver'
               return $ MinMaxICalVersion maxJ minJ (toO o)
parseVersion x = throwError $ "parseVersion: " ++ show x

-- | Parse a TZID property. 3.8.3.1
parseTZID :: Content -> ContentParser TZID
parseTZID (ContentLine _ "TZID" o bs) = do
    tzidValue <- asks $ ($ bs) . dfBS2Text
    let tzidGlobal = (fst <$> T.uncons tzidValue) == Just '/'
        tzidOther = toO o
    return TZID {..}
parseTZID x = throwError $ "parseTZID: " ++ show x

-- | Parse RRule. 3.8.5.3
parseRRule :: Maybe DTStart -> Content -> ContentParser RRule
parseRRule Nothing _ = throwError "parseRRule: missing DTSTART."
parseRRule (Just dts) (ContentLine _ "RRULE" o bs) =
    case runParser (parseRecur dts) def "RRULE" bs of
         Left e -> throwError $ show e
         Right x -> do y <- x
                       return . RRule y $ toO o
parseRRule _ x = throwError $ "parseRRule: " ++ show x

-- | Parse Created, 3.8.7.3
parseCreated :: Content -> ContentParser Created
parseCreated (ContentLine _ "CREATED" o bs) = do
    createdValue <- mustBeUTC =<< parseDateTime Nothing bs
    let createdOther = toO o
    return Created {..}
parseCreated x = throwError $ "parseCreated: " ++ show x

-- | Parse Last Modified, 3.8.7.3
parseLastModified :: Content -> ContentParser LastModified
parseLastModified (ContentLine _ "LAST-MODIFIED" o bs) = do
    lastModifiedValue <- mustBeUTC =<< parseDateTime Nothing bs
    let lastModifiedOther = toO o
    return LastModified {..}
parseLastModified x = throwError $ "parseLastModified: " ++ show x

-- | Parse an RDate
parseRDate :: Content -> ContentParser RDate
parseRDate c@(ContentLine _ "RDATE" o bs) = do
    typ <- paramOnlyOne . fromMaybe ["DATE-TIME"] $ lookup "VALUE" o
    case typ of
         "PERIOD" -> do
             tzid <- mapM paramOnlyOne $ lookup "TZID" o
             p <- S.fromList .: mapM (parsePeriod tzid) $ B.split ',' bs
             return . RDatePeriods p . toO $
                filter ((`notElem` ["VALUE", "TZID"]) . fst) o
         _ -> parseSimpleDatesOrDateTimes RDateDateTimes RDateDates c
parseRDate x = throwError $ "parseRDate: " ++ show x

-- | Parse a UTC Offset property 3.3.14, 3.8.3.4, and 3.8.3.3
parseUTCOffset :: Content -> ContentParser UTCOffset
parseUTCOffset (ContentLine _ n o bs)
        | n `elem` ["TZOFFSETTO", "TZOFFSETFROM"] = do
    let str = B.unpack bs
        (s:rest) = str
        (t1:t2:m1:m2:sec) = map digitToInt rest
        (s1:s2:_) = sec
        sign x = if s == '-' then negate x else x
    when (length str < 5 || any (not . isDigit) rest || s `notElem` ['+','-']
                         || length sec `notElem` [0,2]) .
        throwError $ "parseUTCOffset: " ++ str
    return . UTCOffset (sign $ ((t1 * 10 + t2) * 60 + (m1 * 10 + m2)) * 60 +
                                if not (null sec) then s1 * 10 + s2 else 0)
                       $ toO o
parseUTCOffset x = throwError $ "parseUTCOffset: " ++ show x

-- }}}
