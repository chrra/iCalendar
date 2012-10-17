{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Text.ICalendar.Parser
    ( parseICal
    , parseICalFile
    , DecodingFunctions(..)
    ) where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Arrow (second)
import Control.Monad hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Control.Monad.RWS ( RWS, runRWS, MonadWriter(tell)
                         , MonadState(get, put), asks, modify)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Builder as Bu
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.Default
import Data.List (partition)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import Data.Time ( UTCTime(UTCTime), LocalTime(LocalTime), Day
                 , TimeOfDay())
import qualified Data.Time as Time
import Data.Traversable (mapM)
import qualified Data.Version as Ver
import qualified Network.URI as URI
import qualified System.Locale as L
import Text.ParserCombinators.ReadP (readP_to_S)

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type (mimeType, MIMEType)
import qualified Data.ByteString.Base64.Lazy as B64
import Text.Parsec.Prim hiding (many, (<|>))
import Text.Parsec.Pos
import Text.Parsec.Perm
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.ByteString.Lazy ()
import Text.Parsec.Text.Lazy ()
import qualified Text.Parsec as P

import Text.ICalendar.Types


-- | This module use a specialized line-unfolding parser.
-- See 'scan'.
type TextParser = P.Parsec ByteString DecodingFunctions

type ContentParser = ErrorT String -- Fatal errors.
                            (RWS DecodingFunctions
                                 [String] -- Warnings.
                                 (P.SourcePos, [Content]))

data DecodingFunctions = DecodingFunctions
    { dfBS2Text :: ByteString -> Text
    , dfBS2IText :: ByteString -> CI Text
    }

instance Default DecodingFunctions where
    def = DecodingFunctions TE.decodeUtf8 (CI.mk . TE.decodeUtf8)

data Content = ContentLine SourcePos (CI Text) [(CI Text, [Text])] ByteString
             | Component SourcePos (CI Text) [Content]
               deriving (Show, Eq, Ord)


-- | Parse a ByteString containing iCalendar data.
parseICal :: DecodingFunctions
          -> FilePath
          -> ByteString
          -> Either String (VCalendar, [String])
parseICal s f bs = do
    a <- either (Left . show) Right $ runParser parseToContent s f bs
    when (null a) $ throwError "Missing content."
    unless (null $ drop 1 a) $ throwError "Multiple top-level components."
    let (g, (pos, _), w) = runCP s $ parseVCalendar (head a)
    x <- case g of
              Left e -> Left $ show pos ++ ": " ++ e
              Right x -> Right x
    return (x, w)

-- | Parse an iCalendar file.
parseICalFile :: DecodingFunctions
              -> FilePath
              -> IO (Either String (VCalendar, [String]))
parseICalFile s f = parseICal s f <$> B.readFile f

-- {{{ Parse into content.

parseToContent :: TextParser [Content]
parseToContent = do content <- sepEndBy1 contentline newline
                    f <- dfBS2IText <$> getState
                    return $ componentalize f content

newline :: TextParser ()
newline = (char '\r' >> void (optional $ char '\n')) <|> void (char '\n')


componentalize :: (ByteString -> CI Text) -> [Content] -> [Content]
componentalize f (ContentLine p "BEGIN" [] n:xs) =
    let (com, rest) = break g xs
        g (ContentLine _ "END" [] en) | f en == n' = True
        g _ = False
        n' = f n
     in Component p n' (componentalize f com)
                : componentalize f (drop 1 rest)
componentalize f (x:xs) = x:componentalize f xs
componentalize _ _ = []


-- | Specialized scan function which unfolds lines.
scan :: s -- ^ Initial state.
     -> (s -> Maybe Char -> Maybe (Maybe s))
     -- ^ Nothing: Fail.
     -- Just Nothing: Done, don't use last char.
     -- Just (Just state): Continue, collecting char unless EOF.
     -> TextParser ByteString
scan state f = go state mempty
  where go st buf = do
            _ <- many (try unfold)
            c <- lookAhead (Just <$> P.anyChar <|> Nothing <$ P.eof)
            case (c, f st c) of
                 (_, Nothing) -> mzero
                 (Just c', Just (Just st')) ->
                    P.anyChar *> go st' (buf <> Bu.char8 c')
                 (_, _) -> return $ Bu.toLazyByteString buf

        unfold = (P.char '\r' >> optional (P.char '\n') >> P.oneOf " \t")
             <|> (P.char '\n' >> P.oneOf " \t")

takeWhile1 :: (Char -> Bool) -> TextParser ByteString
takeWhile1 p = scan False f <?> "takeWhile1 ..."
  where f g (Just x) | p x       = Just (Just True)
                     | g         = Just Nothing
                     | otherwise = Nothing
        f g _        | g         = Just Nothing
                     | otherwise = Nothing

char :: Char -> TextParser ByteString
char c = scan True f <?> show c
  where f True x = if Just c == x then Just (Just False) else Nothing
        f False _ = Just Nothing


isControl', isSafe, isValue, isQSafe, isName :: Char -> Bool
isControl' c = c /= '\t' && isControl c
isSafe c = not (isControl' c) && c `notElem` "\";:,"
isValue c = let n = fromEnum c in n == 32 || n == 9 || (n >= 0x21 && n /= 0x7F)
isQSafe c = isValue c && c /= '"'
isName c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '-'

contentline :: TextParser Content
contentline = do pos <- getPosition
                 n <- name
                 ps <- many (char ';' >> param)
                 _ <- char ':'
                 val <- value <|> return mempty
                 return $ ContentLine pos n ps val
  where value :: TextParser ByteString
        value = takeWhile1 isValue <?> "value"

        param :: TextParser (CI Text, [Text])
        param = do n <- name
                   _ <- char '='
                   vs <- sepBy1 paramValue (char ',')
                   return (n, vs)

        name :: TextParser (CI Text)
        name = dfBS2IText <$> getState <*> takeWhile1 isName <?> "name"

        paramValue :: TextParser Text
        paramValue = paramtext <|> quotedString

        paramtext :: TextParser Text
        paramtext = dfBS2Text <$> getState <*> takeWhile1 isSafe <?> "paramtext"

        quotedString :: TextParser Text
        quotedString = (do
            _ <- char '"'
            s <- takeWhile1 isQSafe <|> return mempty
            _ <- char '"'
            dfBS2Text <$> getState <*> pure s) <?> "quoted string"

-- }}}
-- {{{ Parse content lines into iCalendar components.

runCP :: DecodingFunctions -> ContentParser a
      -> (Either String a, (SourcePos, [Content]), [String])
runCP s = ((flip .) . flip) runRWS s (undefined, undefined) . runErrorT

-- {{{ Component parsers

-- | Parse a VCALENDAR component. 3.4
parseVCalendar :: Content -> ContentParser VCalendar
parseVCalendar c@(Component _ "VCALENDAR" _) = down c $ do
    vcProdId <- reqLine1 "PRODID" (parseSimple ProdId)
    vcVersion <- reqLine1 "VERSION" parseVersion
    vcScale <- optLine1 "CALSCALE" (parseSimpleI Scale)
    vcMethod <- optLine1 "METHOD" (parseSimpleI ((Just .) . Method))
    vcTimeZones <- optCompN "VTIMEZONE" parseVTimeZone
    vcEvents <- optCompN "VEVENT" parseVEvent
    vcTodos <- optCompN "VTODO" parseVTodo
    vcJournals <- optCompN "VJOURNAL" parseVJournal
    vcFreeBusys <- optCompN "VFREEBUSY" parseVFreeBusy
    vcOtherComps <- otherComponents
    vcOther <- otherProperties
    return VCalendar {..}
parseVCalendar _ = throwError "parseVCalendar: Content given not a VCALENDAR\
                              \ component."

-- | Parse a VEVENT component. 3.6.1
parseVEvent :: Content -> ContentParser VEvent
parseVEvent (Component _ "VEVENT" _) = do
    veDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    veUID <- reqLine1 "UID" $ parseSimple UID
    veDTStart <- optLine1 "DTSTART" $
                  Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    veClass <- optLine1 "CLASS" parseClass
    veCreated <- optLine1 "CREATED" $ parseSimpleDateTime ((Just .) . Created)
    veDescription <- optLine1 "DESCRIPTION" .
                    parseAltRepLang $ (((Just .) .) .) . Description
    veGeo <- optLine1 "GEO" (Just .: parseGeo)
    veLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    veLocation <- optLine1 "LOCATION" .
        parseAltRepLang $ (((Just .) .) .) . Location
    veOrganizer <- optLine1 "ORGANIZER" (Just .: parseOrganizer)
    vePriority <- optLine1 "PRIORITY" (parseSimpleRead Priority)
    veSeq <- optLine1 "SEQUENCE" (parseSimpleRead Sequence)
    veStatus <- optLine1 "STATUS" (Just .: parseEventStatus)
    veSummary <- optLine1 "SUMMARY" .
                    parseAltRepLang $ (((Just .) .) .) . Summary
    veTransp <- optLine1 "TRANSP" parseTransp
    veUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    veRecurId <- optLine1 "RECURRENCE-ID"
                    $ Just .: parseRecurId veDTStart
    veRRule <- optLineN "RRULE" $ parseRRule $ veDTStart
    when (S.size veRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    veDTEndDuration <- parseXDurationOpt "DTEND" DTEndDateTime DTEndDate
                                         $ veDTStart
    veAttach <- optLineN "ATTACH" parseAttachment
    veAttendee <- optLineN "ATTENDEE" parseAttendee
    veCategories <- optLineN "CATEGORIES" parseCategories
    veComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    veContact <- optLineN "CONTACT" $ parseAltRepLang Contact
    veExDate <- optLineN "EXDATE" parseExDate
    veRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    veRelated <- optLineN "RELATED-TO" parseRelatedTo
    veResources <- optLineN "RESOURCES" $ parseAltRepLangN Resources
    veRDate <- optLineN "RDATE" parseRDate
    veAlarms <- optCompN "VALARM" parseVAlarm
    veOther <- otherProperties
    return VEvent {..}
parseVEvent x = throwError $ "parseVEvent: " ++ show x

-- | Parse a VTODO component.
parseVTodo :: Content -> ContentParser VTodo
parseVTodo (Component _ "VTODO" _) = do
    vtDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    vtUID <- reqLine1 "UID" $ parseSimple UID
    vtClass <- optLine1 "CLASS" parseClass
    vtCompleted <- optLine1 "COMPLETED" . parseSimpleDateTime $
                (Just .) . Completed
    vtCreated <- optLine1 "CREATED" . parseSimpleDateTime $
                (Just .) . Created
    vtDTStart <- optLine1 "DTSTART" $
                Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    vtDescription <- optLine1 "DESCRIPTION" .
                parseAltRepLang $ (((Just .) .) .) . Description
    vtGeo <- optLine1 "GEO" (Just .: parseGeo)
    vtLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    vtLocation <- optLine1 "LOCATION" .
                parseAltRepLang $ (((Just .) .) .) . Location
    vtOrganizer <- optLine1 "ORGANIZER" (Just .: parseOrganizer)
    vtPercent <- optLine1 "PERCENT-COMPLETE" $ Just .: parseSimpleRead
                                                             PercentComplete
    vtPriority <- optLine1 "PRIORITY" $ parseSimpleRead Priority
    vtRecurId <- optLine1 "RECURRENCE-ID" (Just .: parseRecurId vtDTStart)
    vtSeq <- optLine1 "SEQUENCE" $ parseSimpleRead Sequence
    vtStatus <- optLine1 "STATUS" (Just .: parseTodoStatus)
    vtSummary <- optLine1 "SUMMARY" .
                parseAltRepLang $ (((Just .) .) .) . Summary
    vtUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    vtRRule <- optLineN "RRULE" $ parseRRule vtDTStart
    when (S.size vtRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    vtDueDuration <- parseXDurationOpt "DUE" DueDateTime DueDate vtDTStart

    vtAttach <- optLineN "ATTACH" parseAttachment
    vtAttendee <- optLineN "ATTENDEE" parseAttendee
    vtCategories <- optLineN "CATEGORIES" parseCategories
    vtComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    vtContact <- optLineN "CONTACT" $ parseAltRepLang Contact
    vtExDate <- optLineN "EXDATE" parseExDate
    vtRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    vtRelated <- optLineN "RELATED-TO" parseRelatedTo
    vtResources <- optLineN "RESOURCES" $ parseAltRepLangN Resources
    vtRDate <- optLineN "RDATE" parseRDate
    vtAlarms <- optCompN "VALARM" parseVAlarm
    vtOther <- otherProperties
    return VTodo {..}
parseVTodo x = throwError $ "parseVTodo: " ++ show x

-- | Parse a VTIMEZONE component. 3.6.5
parseVTimeZone :: Content -> ContentParser VTimeZone
parseVTimeZone (Component _ "VTIMEZONE" _) = do
    vtzId <- reqLine1 "TZID" parseTZID
    vtzLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    vtzUrl <- optLine1 "TZURL" (Just .: parseSimpleURI TZUrl)
    vtzStandardC <- optCompN "STANDARD" parseTZProp
    vtzDaylightC <- optCompN "DAYLIGHT" parseTZProp
    when (S.size vtzStandardC + S.size vtzDaylightC < 1) .
        throwError $ "VTIMEZONE must include at least one of the STANDARD or \
                     \DAYLIGHT components."
    vtzOther <- otherProperties
    return VTimeZone {..}
parseVTimeZone x = throwError $ "parseVTimeZone: " ++ show x

-- | Parse a STANDARD or DAYLIGHT component, tzprop. 3.6.5
parseTZProp :: Content -> ContentParser TZProp
parseTZProp (Component _ n _) | n `elem` ["STANDARD", "DAYLIGHT"] = do
    tzpDTStart <- reqLine1 "DTSTART" $
                    parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    tzpTZOffsetTo <- reqLine1 "TZOFFSETTO" parseUTCOffset
    tzpTZOffsetFrom <- reqLine1 "TZOFFSETFROM" parseUTCOffset
    tzpRRule <- optLineN "RRULE" (parseRRule $ Just tzpDTStart)
    when (S.size tzpRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    tzpComment <- optLineN "COMMENT" (parseAltRepLang Comment)
    tzpRDate <- optLineN "RDATE" parseRDate
    tzpTZName <- optLineN "TZNAME" parseTZName
    tzpOther <- otherProperties
    return TZProp {..}
parseTZProp x = throwError $ "parseTZProp: " ++ show x

-- | Parse a VALARM component. 3.6.6
parseVAlarm :: Content -> ContentParser VAlarm
parseVAlarm (Component _ "VALARM" _) = do
    (ao, a') <- reqLine1 "ACTION" (\(ContentLine _ _ o bs) -> return (o, bs))
    a <- valueOnlyOne =<< parseText a'
    vaTrigger <- reqLine1 "TRIGGER" parseTrigger
    let vaActionOther = toO ao
    case CI.mk a of
         "AUDIO"   -> do
            (vaRepeat, vaDuration) <- repAndDur
            vaAudioAttach <- optLine1 "ATTACH" $ Just .: parseAttachment
            vaOther <- otherProperties
            return VAlarmAudio {..}
         "DISPLAY" -> do
             (vaRepeat, vaDuration) <- repAndDur
             vaDescription <- reqLine1 "DESCRIPTION" $
                 parseAltRepLang Description
             vaOther <- otherProperties
             return VAlarmDisplay {..}
         "EMAIL"   -> do
             (vaRepeat, vaDuration) <- repAndDur
             vaDescription <- reqLine1 "DESCRIPTION" $
                parseAltRepLang Description
             vaSummary <- reqLine1 "SUMMARY" $ parseAltRepLang Summary
             vaAttendee <- reqLineN "ATTENDEE" parseAttendee
             vaMailAttach <- optLineN "ATTACH" parseAttachment
             vaOther <- otherProperties
             return VAlarmEmail {..}
         vaAction  -> do vaOther <- otherProperties
                         return VAlarmX {..}
  where repAndDur = do
             rep <- optLine1 "REPEAT" $ parseSimpleRead Repeat
             dur <- optLine1 "DURATION" $ Just .: parseDurationProp Nothing
             -- Liberal interpretation:
             when (repeatValue rep > 0 && isNothing dur) .
                 throwError $ "parseVAlarm: when REPEAT > 0, DURATION must \
                              \ be specified."
             return (rep, dur)
parseVAlarm x = throwError $ "parseVAlarm: " ++ show x

parseVJournal :: Content -> ContentParser VJournal
parseVJournal (Component _ "VJOURNAL" _) = do
    vjDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    vjUID <- reqLine1 "UID" $ parseSimple UID
    vjClass <- optLine1 "CLASS" parseClass
    vjCreated <- optLine1 "CREATED" . parseSimpleDateTime $
                (Just .) . Created
    vjDTStart <- optLine1 "DTSTART" $
                Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    vjDescription <- optLineN "DESCRIPTION" $ parseAltRepLang Description
    vjLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    vjOrganizer <- optLine1 "ORGANIZER" (Just .: parseOrganizer)
    vjRecurId <- optLine1 "RECURRENCE-ID" (Just .: parseRecurId vjDTStart)
    vjSeq <- optLine1 "SEQUENCE" $ parseSimpleRead Sequence
    vjStatus <- optLine1 "STATUS" (Just .: parseJournalStatus)
    vjSummary <- optLine1 "SUMMARY" .
                parseAltRepLang $ (((Just .) .) .) . Summary
    vjUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    vjRRule <- optLineN "RRULE" $ parseRRule vjDTStart
    when (S.size vjRRule > 1) $ tell ["SHOULD NOT have multiple RRules."]
    vjAttach <- optLineN "ATTACH" parseAttachment
    vjAttendee <- optLineN "ATTENDEE" parseAttendee
    vjCategories <- optLineN "CATEGORIES" parseCategories
    vjComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    vjContact <- optLineN "CONTACT" $ parseAltRepLang Contact
    vjExDate <- optLineN "EXDATE" parseExDate
    vjRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    vjRelated <- optLineN "RELATED-TO" parseRelatedTo
    vjRDate <- optLineN "RDATE" parseRDate
    vjOther <- otherProperties
    return VJournal {..}
parseVJournal x = throwError $ "parseVJournal: " ++ show x

parseVFreeBusy :: Content -> ContentParser VFreeBusy
parseVFreeBusy (Component _ "VFreeBusy" _) = do
    vfbDTStamp <- reqLine1 "DTSTAMP" $ parseSimpleUTC DTStamp
    vfbUID <- reqLine1 "UID" $ parseSimple UID
    vfbContact <- optLine1 "CONTACT" $ Just .: parseAltRepLang Contact
    vfbDTStart <- optLine1 "DTSTART" $
                  Just .: parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    vfbDTEnd <- optLine1 "DTEND" $ Just .: parseSimpleDateOrDateTime
                                                DTEndDateTime DTEndDate
    vfbOrganizer <- optLine1 "ORGANIZER" $ Just .: parseOrganizer
    vfbAttendee <- optLineN "ATTENDEE" parseAttendee
    vfbComment <- optLineN "COMMENT" $ parseAltRepLang Comment
    vfbRStatus <- optLineN "REQUEST-STATUS" parseRequestStatus
    vfbUrl <- optLine1 "URL" (Just .: parseSimpleURI URL)
    vfbFreeBusy <- optLineN "FREEBUSY" parseFreeBusy
    vfbOther <- otherProperties
    return VFreeBusy {..}
parseVFreeBusy x = throwError $ "parseVFreeBusy: " ++ show x

otherComponents :: ContentParser (Set VOther)
otherComponents = optN parseVOther . partition isComponent =<< snd <$> get

parseVOther :: Content -> ContentParser VOther
parseVOther (Component _ voName _) = do
    voProps <- otherProperties
    return VOther {..}
parseVOther x = throwError $ "parseVOther: "++ show x

-- }}}
-- {{{ Property parsers

parseFreeBusy :: Content -> ContentParser FreeBusy
parseFreeBusy (ContentLine _ "FREEBUSY" o bs) = do
    typ <- maybe (return def) (parseFBType . CI.mk .: paramOnlyOne) $
                     lookup "FBTYPE" o
    periods <- S.fromList .: mapM parsePeriod $ B.split ',' bs
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
    typ <- paramOnlyOne . fromMaybe ["DATE-TIME"] $ lookup "VALUE" o
    tzid <- mapM paramOnlyOne $ lookup "TZID" o
    let f x = x /= "VALUE" && if typ == "DATE-TIME" then x /= "TZID"
                                                    else True
        o' = filter (f . fst) o
        bs' = B.split ',' bs
    case typ of
         "DATE-TIME" -> do xs <- mapM (parseDateTime tzid) bs'
                           return . ExDateTime (S.fromList xs) $ toO o'
         "DATE" -> do xs <- mapM parseDate bs'
                      return . ExDate (S.fromList xs) $ toO o'
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
  where g = maybe $ return def
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
    range' <- maybe (return def) (parseRange . CI.mk <=< paramOnlyOne) $
                                   lookup "RANGE" o
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
        lat = maybeRead $ B.unpack lat' :: Maybe Float
        long = maybeRead . B.unpack $ B.tail long' :: Maybe Float
    when (B.null long' || isNothing (lat >> long)) .
        throwError $ "Invalid latitude/longitude: " ++ show bs
    return $ Geo (fromJust lat) (fromJust long) (toO o)
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
             p <- S.fromList .: mapM parsePeriod $ B.split ',' bs
             return $ RDatePeriods p (toO $ filter ((/="VALUE") . fst) o)
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
    when (length str < 5 || any (not . isDigit) rest || s `notElem` "+-"
                         || length sec `notElem` [0,2]) .
        throwError $ "parseUTCOffset: " ++ str
    return . UTCOffset (sign $ ((t1 * 10 + t2) * 60 + (m1 * 10 + m2)) * 60 +
                                if not (null sec) then s1 * 10 + s2 else 0)
                       $ toO o
parseUTCOffset x = throwError $ "parseUTCOffset: " ++ show x

-- }}}
-- {{{ Value/Parameter parsers.

parseAlarmTriggerRelationship :: CI Text
                              -> ContentParser AlarmTriggerRelationship
parseAlarmTriggerRelationship "START" = return Start
parseAlarmTriggerRelationship "END" = return End
parseAlarmTriggerRelationship x =
    throwError $ "parseAlarmTriggerRelationship: " ++ show x

-- | Parse relationship type. 3.2.15
parseRelationshipType :: CI Text -> RelationshipType
parseRelationshipType "PARENT"  = Parent
parseRelationshipType "CHILD"   = Child
parseRelationshipType "SIBLING" = Sibling
parseRelationshipType x = RelationshipTypeX x

-- | Parse bool. 3.3.2
parseBool :: CI Text -> ContentParser Bool
parseBool "TRUE" = return True
parseBool "FALSE" = return False
parseBool x = throwError $ "parseBool: " ++ show x

-- | Parse recurrence identifier range. 3.2.13
parseRange :: CI Text -> ContentParser Range
parseRange "THISANDFUTURE" = return ThisAndFuture
parseRange "THISANDPRIOR"  = do tell ["THISANDPRIOIR RANGE is deprecated."]
                                return ThisAndPrior
parseRange x = throwError $ "parseRange: " ++ show x

-- | Parse free/busy time type. 3.2.9.
parseFBType :: CI Text -> FBType
parseFBType "FREE"             = Free
parseFBType "BUSY"             = Busy
parseFBType "BUSY-UNAVAILABLE" = BusyUnavailable
parseFBType "BUSY-TENTATIVE"   = BusyTentative
parseFBType x                  = FBTypeX x

-- | Parse participation status. 3.2.12
parsePartStat :: CI Text -> PartStat
parsePartStat "NEEDS-ACTION" = PartStatNeedsAction
parsePartStat "ACCEPTED" = Accepted
parsePartStat "DECLINED" = Declined
parsePartStat "TENTATIVE" = Tentative
parsePartStat "DELEGATED" = Delegated
parsePartStat "COMPLETED" = PartStatCompleted
parsePartStat "IN-PROCESS" = InProcess
parsePartStat x = PartStatX x

-- | Parse role.
parseRole :: CI Text -> Role
parseRole "CHAIR" = Chair
parseRole "REQ-PARTICIPANT" = ReqParticipant
parseRole "OPT-PARTICIPANT" = OptParticipant
parseRole "NON-PARTICIPANT" = NonParticipant
parseRole x = RoleX x


parseCUType :: CI Text -> CUType
parseCUType "INDIVIDUAL" = Individual
parseCUType "GROUP" = Group
parseCUType "RESOURCE" = Resource
parseCUType "ROOM" = Room
parseCUType "UNKNOWN" = Unknown
parseCUType x = CUTypeX x

parseMime :: Text -> ContentParser MIMEType
parseMime t = let m = mimeType .: parseMIMEType $ T.unpack t
               in maybe (throwError $ "parseMime: " ++ show t) return m

parseURI :: String -> ContentParser URI.URI
parseURI s = case URI.parseURI s of
                  Just x -> return x
                  Nothing -> throwError $  "Invalid URI: " ++ show s

-- | Parse Duration. 3.3.6
parseDuration :: String -- ^ Parser context.
              -> ByteString -- ^ What to parse.
              -> ContentParser Duration
parseDuration what bs =
    case runParser dur def what bs of
         Left e -> throwError $ "Invalid duration: "
                              ++ unlines [show bs, show e]
         Right x -> return x
  where dur = do si <- sign
                 _ <- P.char 'P'
                 day <- optional . try $ digits <* P.char 'D'
                 time <- optional $ do
                    _ <- P.char 'T'
                    h <- optional . try $ digits <* P.char 'H'
                    m <- optional . try $ digits <* P.char 'M'
                    s <- optional . try $ digits <* P.char 'S'
                    return (h, m, s)
                 week <- optional . try $ digits <* P.char 'W'
                 P.eof
                 case (day, time, week) of
                      (Just d, x, Nothing) ->
                           let (h, m, s) = deMHms x
                            in return $ DurationDate si d h m s
                      (Nothing, x@(Just _), Nothing) ->
                           let (h, m, s) = deMHms x
                            in return $ DurationTime si h m s
                      (Nothing, Nothing, Just w) ->
                           return $ DurationWeek si w
                      (_, _, _) -> fail "Invalid."

        sign = fromMaybe Positive <$> optional (Positive <$ P.char '+'
                                            <|> Negative <$ P.char '-')

        deMHms (Just (h, m, s)) = (fromMaybe 0 h, fromMaybe 0 m, fromMaybe 0 s)
        deMHms Nothing = (0, 0, 0)

-- | Parse Recur-value. 3.3.10.
-- Partly implemented in parsec.
parseRecur :: DTStart -> TextParser (ContentParser Recur)
parseRecur dts =
    permute (mkRecur <$$> (freq <* term)
                     <|?> (Nothing, untilCount <* term)
                     <|?> (1, istring "INTERVAL=" *> digits <* term)
                     <|?> ([], istring "BYSECOND=" *> digitsN <* term)
                     <|?> ([], istring "BYMINUTE=" *> digitsN <* term)
                     <|?> ([], istring "BYHOUR=" *> digitsN <* term)
                     <|?> ([], istring "BYDAY=" *> sepBy wday (P.char ',')
                                                     <* term)
                     <|?> ([], istring "BYMONTHDAY=" *> onum <* term)
                     <|?> ([], istring "BYYEARDAY=" *> onum <* term)
                     <|?> ([], istring "BYWEEKNO=" *> onum <* term)
                     <|?> ([], istring "BYMONTH=" *> digitsN <* term)
                     <|?> ([], istring "BYSETPOS=" *> onum <* term)
                     <|?> (Monday, istring "WKST=" *> weekday <* term))
      <* P.eof

  where freq = istring "FREQ=" *> frequency
        frequency = Secondly <$ istring "SECONDLY"
                <|> Minutely <$ istring "MINUTELY"
                <|> Hourly   <$ istring "HOURLY"
                <|> Daily    <$ istring "DAILY"
                <|> Weekly   <$ istring "WEEKLY"
                <|> Monthly  <$ istring "MONTHLY"
                <|> Yearly   <$ istring "YEARLY"
        weekday = Sunday    <$ istring "SU"
              <|> Monday    <$ istring "MO"
              <|> Tuesday   <$ istring "TU"
              <|> Wednesday <$ istring "WE"
              <|> Thursday  <$ istring "TH"
              <|> Friday    <$ istring "FR"
              <|> Saturday  <$ istring "SA"
        wday = Right <$> weekday
           <|> (Left .) . (,) <$> (neg <*> digits) <*> weekday
        onum = sepBy1 (neg <*> digits) (P.char ',')
        untilCount = istring "UNTIL=" *> until'
                 <|> istring "COUNT=" *> (Just . Right <$> digits)
        until' = do txt <- manyTill P.anyChar (void (P.char ';') <|> P.eof)
                    return . Just . Left $
                        case dts of
                             DTStartDateTime _ _ ->
                                 Right <$> parseDateTime Nothing (B.pack txt)
                             DTStartDate _ _ ->
                                 Left <$> parseDate (B.pack txt)
        term = optional (P.char ';')
        istring :: String -> TextParser ()
        istring = void . try . mapM (\c -> P.char c <|> P.char (toLower c))

        mkRecur f uc i s m h d md yd wn mo sp wkst = do
            uc' <- case uc of
                        Just (Left x) -> Just . Left <$> x
                        Just (Right y) -> return . Just $ Right y
                        Nothing -> return Nothing
            return $ Recur f uc' i s m h d md yd wn mo sp wkst

-- | Parse text. 3.3.11
parseText' :: ByteString -> ContentParser ([Text], ByteString)
parseText' bs = do c <- asks dfBS2Text
                   case runParser ((,) <$> texts <*> getInput) () "text" bs of
                        Left e -> throwError $ "parseText': " ++ show e
                        Right (x, r) -> return ( map (c . Bu.toLazyByteString) x
                                               , r)
  where texts = sepBy text (P.char ',')
        text = do x <- P.satisfy isTSafe'
                  case x of
                       '\\' -> do y <- P.anyChar
                                  case y of
                                       '\\' -> nxt '\\'
                                       ';' -> nxt ';'
                                       ',' -> nxt ','
                                       z | z `elem` "nN" -> nxt '\n'
                                       _ -> fail $ "unexpected " ++ show x
                       y -> nxt y
        -- isTSafe + 0x22, 0x3A
        isTSafe' c = let n = ord c
                      in n == 9 || (n >= 0x20 && n <= 0x3A)
                                || (n >= 0x3C && n /= 0x7F)
        nxt c = (Bu.char8 c <>) <$> (text <|> return mempty)

-- | Chech that there's no remainding text after the parser is done.
noRestText :: ([Text], ByteString) -> ContentParser [Text]
noRestText (x, "") = return x
noRestText (_, x) = throwError $ "noRestText: remainding text: " ++ show x

-- | Parse text, not allowing any remainding text.
parseText :: ByteString -> ContentParser [Text]
parseText = noRestText <=< parseText'

-- | Parse a string to a Day. 3.3.4
parseDateStr :: String -> Maybe (Day, String)
parseDateStr = lastToMaybe . Time.readsTime L.defaultTimeLocale "%Y%m%d"

-- | Parse a string to a TimeOfDay, and a bool if it's in UTC.
parseTimeStr :: String -> Maybe (TimeOfDay, Bool)
parseTimeStr s = do
    (t, r) <- lastToMaybe (Time.readsTime L.defaultTimeLocale "%H%M%S" s)
    case r of
         "Z" -> return (t, True)
         "" -> return (t, False)
         _ -> fail ""


-- | Parse a Date value. 3.3.4
parseDate :: ByteString -> ContentParser Date
parseDate bs = do
    str <- asks $ T.unpack . ($ bs) . dfBS2Text
    let dayRes = parseDateStr str
        Just (day, rest) = dayRes
    when (isNothing dayRes) .
        throwError $ "parseDate: " ++ str
    unless (null rest) $
        tell ["parseDate: extra content: " ++ rest]
    return $ Date day

-- | Parse a DateTime value. 3.3.5
parseDateTime :: Maybe Text -- ^ Time Zone ID
              -> ByteString -> ContentParser DateTime
parseDateTime mTZ bs = do
    str <- asks $ T.unpack . ($ bs) . dfBS2Text
    let dayRes = parseDateStr str
        Just (day, rest') = dayRes
        t = take 1 rest'
        timeRes = parseTimeStr $ drop 1 rest'
        Just (time, isUTC) = timeRes
    when (isNothing (dayRes >> timeRes) || t /= "T") .
        throwError $ "parseDateTime: " ++ str
    when (isUTC && isJust mTZ) $
        tell ["parseDateTime: TZID on UTC timezone: " ++ str]
    return $ case (mTZ, isUTC) of
                  (Nothing, False) -> FloatingDateTime (LocalTime day time)
                  (Just tz, False) -> ZonedDateTime (LocalTime day time) tz
                  (_, True) -> UTCDateTime (UTCTime day
                                                  $ Time.timeOfDayToTime time)


parsePeriod :: ByteString -> ContentParser Period
parsePeriod bs = do
    let (dateTime', x) = B.drop 1 <$> B.break (=='/') bs
    when (B.null x) . throwError $ "Invalid period: " ++ show bs
    dateTime <- mustBeUTC =<< parseDateTime Nothing dateTime'
    case B.head x of
         z | z `elem` "+-P" -> PeriodDuration dateTime
                                    <$> parseDuration "period" x
         _ -> PeriodDates dateTime <$> (mustBeUTC =<< parseDateTime Nothing x)

-- }}}
-- {{{ Misc parsers

-- | Convert a 'DateTime' to 'UTCTime', giving an appropriate error.
mustBeUTC :: DateTime -> ContentParser UTCTime
mustBeUTC (UTCDateTime x) = return x
mustBeUTC _ = throwError "DateTime-value must be UTC"

-- | Parse something simple with only a Text-field for the content, and
-- 'OtherParams'.
parseSimple :: (Text -> OtherParams -> b) -> Content -> ContentParser b
parseSimple k (ContentLine _ _ o bs) = do c <- valueOnlyOne =<< parseText bs
                                          return $ k c (toO o)
parseSimple _ x = throwError $ "parseSimple: " ++ show x

-- | Parse something simple with only a CI Text-field for the content, and
-- 'OtherParams'.
parseSimpleI :: (CI Text -> OtherParams -> b) -> Content -> ContentParser b
parseSimpleI k (ContentLine _ _ o bs) = do c <- asks dfBS2IText
                                           return $ k (c bs) (toO o)
parseSimpleI _ x = throwError $ "parseSimpleI: " ++ show x

-- | Parse something simple with only a Int-field for the content, and
-- 'OtherParams'.
parseSimpleRead :: forall a b. Read a
                => (a -> OtherParams -> b) -> Content -> ContentParser b
parseSimpleRead k (ContentLine _ _ o bs) = do
    let r = maybeRead $ B.unpack bs :: Maybe a
    when (isNothing r) . throwError $ "parseSimpleRead: " ++ show bs
    return $ k (fromJust r) (toO o)
parseSimpleRead _ x = throwError $ "parseSimpleRead: " ++ show x

-- | Parse something b with alternative representations, language
-- specification, and 'OtherParams'.
parseAltRepLang' :: ([Text] -> ContentParser b)
                 -> (b -> Maybe URI.URI -> Maybe Language -> OtherParams -> a)
                 -> Content -> ContentParser a
parseAltRepLang' m f (ContentLine _ _ o bs) = do
    t <- m =<< parseText bs
    uri <- mapM (parseURI <=< paramOnlyOne) $ T.unpack .: lookup "ALTREP" o
    lang <- mapM paramOnlyOne $ Language . CI.mk .: lookup "LANGUAGE" o
    let o' = filter (\(x, _) -> x `notElem` ["ALTREP", "LANGUAGE"]) o
    return $ f t uri lang (toO o')
parseAltRepLang' _ _ x = throwError $ "parseAltRepLang': " ++ show x

-- | Parse something 'Text' with alternative representations, language
-- specification, and 'OtherParams'.
parseAltRepLang :: (Text -> Maybe URI.URI -> Maybe Language -> OtherParams -> a)
                -> Content -> ContentParser a
parseAltRepLang = parseAltRepLang' valueOnlyOne

-- | Parse something '[Text]' with alternative representations, language
-- specification, and 'OtherParams'.
parseAltRepLangN :: (Set Text -> Maybe URI.URI -> Maybe Language
                              -> OtherParams -> a)
                -> Content -> ContentParser a
parseAltRepLangN = parseAltRepLang' (return . S.fromList)

-- | Parse something simple with only a URI-field for the content, and
-- 'OtherParams'.
parseSimpleURI :: (URI.URI -> OtherParams -> a) -> Content -> ContentParser a
parseSimpleURI f (ContentLine _ _ o bs) = do
    uri <- parseURI =<< asks (T.unpack . ($ bs) . dfBS2Text)
    return . f uri $ toO o
parseSimpleURI _ x = throwError $ "parseSimpleURI: " ++ show x

-- | Parse something which has either a 'Date' or a 'DateTime' value, and
-- 'OtherParams'. Uses DateTime if there is no value parameter.
parseSimpleDateOrDateTime :: (DateTime -> OtherParams -> a)
                          -> (Date     -> OtherParams -> a)
                          -> Content
                          -> ContentParser a
parseSimpleDateOrDateTime dt d (ContentLine _ _ o bs) = do
    typ <- paramOnlyOne . fromMaybe ["DATE-TIME"] $ lookup "VALUE" o
    tzid <- mapM paramOnlyOne $ lookup "TZID" o
    let f x = x /= "VALUE" && if typ == "DATE-TIME" then x /= "TZID"
                                                    else True
        o' = filter (f . fst) o
    case typ of
         "DATE-TIME" -> do x <- parseDateTime tzid bs
                           return . dt x $ toO o'
         "DATE" -> do x <- parseDate bs
                      return . d x $ toO o'
         _ -> throwError $ "Invalid type: " ++ show typ
parseSimpleDateOrDateTime _ _ x =
    throwError $ "parseSimpleDateOrDateTime: " ++ show x

-- | Parse something which has a set of either a 'Date' or a 'DateTime' value,
-- and 'OtherParams'. Uses DateTime if there is no value parameter.
parseSimpleDatesOrDateTimes :: (Set DateTime -> OtherParams -> a)
                            -> (Set Date     -> OtherParams -> a)
                            -> Content
                            -> ContentParser a
parseSimpleDatesOrDateTimes dt d (ContentLine _ _ o bs) = do
    typ <- paramOnlyOne . fromMaybe ["DATE-TIME"] $ lookup "VALUE" o
    tzid <- mapM paramOnlyOne $ lookup "TZID" o
    let f x = x /= "VALUE" && if typ == "DATE-TIME" then x /= "TZID"
                                                    else True
        o' = filter (f . fst) o
    case typ of
         "DATE-TIME" -> do x <- S.fromList .: mapM (parseDateTime tzid) $
                                                B.split ',' bs
                           return . dt x $ toO o'
         "DATE" -> do x <- S.fromList .: mapM parseDate $ B.split ',' bs
                      return . d x $ toO o'
         _ -> throwError $ "Invalid type: " ++ show typ
parseSimpleDatesOrDateTimes _ _ x =
    throwError $ "parseSimpleDatesOrDateTimes: " ++ show x

-- | Parse something which has only a DateTime value, and 'OtherParams'.
parseSimpleDateTime :: (DateTime -> OtherParams -> a)
                    -> Content
                    -> ContentParser a
parseSimpleDateTime dt (ContentLine _ _ o bs) = do
    tzid <- mapM paramOnlyOne $ lookup "TZID" o
    let o' = filter ((/="TZID") . fst) o
    flip dt (toO o') <$> parseDateTime tzid bs
parseSimpleDateTime _ x = throwError $ "parseSimpleDateTime: " ++ show x

parseSimpleUTC :: (UTCTime -> OtherParams -> a)
               -> Content
               -> ContentParser a
parseSimpleUTC dt (ContentLine _ _ o bs) =
    flip dt (toO o) <$> (mustBeUTC =<< parseDateTime Nothing bs)
parseSimpleUTC _ x = throwError $ "parseSimpleUTC: " ++ show x

-- | Convert a property dictionary to 'OtherParams'.
toO :: [(CI Text, [Text])] -> OtherParams
toO = OtherParams . S.fromList . map (uncurry OtherParam)

-- | Get the remaining properties.
otherProperties :: ContentParser (Set OtherProperty)
otherProperties = do opts <- snd <$> get
                     modify (second $ const [])
                     S.fromList <$> mapM lineToOtherProp opts
  where lineToOtherProp (ContentLine _ n opts bs) =
            return (OtherProperty n bs $ toO opts)
        lineToOtherProp c@Component {} =
            down c . throwError $ "Unconsumed component: " ++ show c

neg :: TextParser (Int -> Int)
neg = maybe id (\x -> if x == '-' then negate else id)
            <$> optional (P.oneOf "+-")

digits :: TextParser Int
digits = foldl1 ((+).(*10)) . map digitToInt <$> many1 P.digit

digitsN :: TextParser [Int]
digitsN = sepBy1 digits (P.char ',')

-- }}}
-- {{{ Combinators

-- | Set the parser context.
down :: Content -> ContentParser a -> ContentParser a
down (Component p _ x) = down' (p, x)
down x@(ContentLine p _ _ _) = down' (p, [x])

-- | Set the parser context.
down' :: (P.SourcePos, [Content]) -> ContentParser a -> ContentParser a
down' x m = get >>= \old -> put x >> m <* put old

-- | Many optional components named ...
optCompN :: Ord a
         => CI Text -> (Content -> ContentParser a) -> ContentParser (Set a)
optCompN s f = optN f . partition (`isComponentNamed` s) =<< snd <$> get

-- | One required line named ...
reqLine1 :: CI Text -> (Content -> ContentParser a) -> ContentParser a
reqLine1 s f = req1 s f . partition (`isLineNamed` s) =<< snd <$> get

-- | One optional line named ...
optLine1 :: Default b
         => CI Text -> (Content -> ContentParser b) -> ContentParser b
optLine1 s f = opt1 f . partition (`isLineNamed` s) =<< snd <$> get

-- | Many optional lines named ...
optLineN :: Ord b
         => CI Text -> (Content -> ContentParser b) -> ContentParser (Set b)
optLineN s f = optN f . partition (`isLineNamed` s) =<< snd <$> get

-- | Many lines named ..., at least one required.
reqLineN :: Ord b
         => CI Text -> (Content -> ContentParser b) -> ContentParser (Set b)
reqLineN s f = reqN s f . partition (`isLineNamed` s) =<< snd <$> get

-- | One required ...
req1 :: CI Text -> (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser b
req1 _ f ([x], xs) = modify (second $ const xs) >> down x (f x)
req1 s _ ([], _) = throwError $ "Missing content: " ++ show s
req1 _ f (x:xs, xs') = do modify (second $ const xs')
                          tell (map (("Extra content: " ++) . show) xs)
                          down x $ f x

-- | One optional ...
opt1 :: Default b
     => (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser b
opt1 f ([x], xs) = modify (second $ const xs) >> down x (f x)
opt1 _ ([], _) = return def
opt1 f (x:xs, xs') = do modify (second $ const xs')
                        tell (map (("Extra content: " ++) . show) xs)
                        down x $ f x

-- | Many optional ...
optN :: Ord b
     => (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser (Set b)
optN f (xs, xs') = do modify (second $ const xs')
                      S.fromList <$> mapM (\x -> down x (f x)) xs

-- | Many ..., at least one required.
reqN :: Ord b
     => CI Text -- ^ What, needed for the error.
     -> (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser (Set b)
reqN w f (xs, xs') = do modify (second $ const xs')
                        o <- S.fromList <$> mapM (\x -> down x (f x)) xs
                        when (S.size o < 1) .
                            throwError $ "At least one required: " ++ show w
                        return o

-- | Only allow one parameter value.
paramOnlyOne :: [a] -> ContentParser a
paramOnlyOne [x] = return x
paramOnlyOne _ = throwError "Only one parameter value allowed."

valueOnlyOne :: [a] -> ContentParser a
valueOnlyOne [x] = return x
valueOnlyOne _ = throwError "Only one value allowed."

-- | Line predicate.
isLineNamed :: Content -> CI Text -> Bool
isLineNamed (ContentLine _ n _ _) n' | n == n' = True
isLineNamed _ _ = False

-- | Component name predicate.
isComponentNamed :: Content -> CI Text -> Bool
isComponentNamed (Component _ n _) n' | n == n' = True
isComponentNamed _ _ = False

isComponent :: Content -> Bool
isComponent (Component _ _ _) = True
isComponent _ = False

-- }}}
-- }}}
-- {{{ Util

maybeRead :: Read a => String -> Maybe a
maybeRead = fst .: lastToMaybe . reads

lastToMaybe :: [a] -> Maybe a
lastToMaybe x = if null x then Nothing else Just $ last x

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap fmap fmap

infixl 4 .:

-- }}}
