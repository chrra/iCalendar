{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Text.ICalendar.Parser where

import Prelude hiding (mapM)
import Control.Applicative
import Control.Arrow (second)
import Control.Monad hiding (mapM)
import Control.Monad.Error hiding (mapM)
import Control.Monad.RWS ( RWS, RWST(..), runRWS, MonadReader(ask)
                         , MonadWriter(tell), MonadState(get, put)
                         , asks, modify)
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
                 , TimeOfDay(TimeOfDay))
import qualified Data.Time as Time
import Data.Traversable (mapM)
import qualified Data.Version as Ver
import qualified Network.URI as URI
import qualified System.Locale as L
import Text.ParserCombinators.ReadP (readP_to_S)

import Text.Parsec.Prim hiding (many, (<|>))
import Text.Parsec.Pos
import Text.Parsec.Perm
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.ByteString.Lazy ()
import qualified Text.Parsec as P

import Text.ICalendar.Types

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap fmap fmap

infixl 4 .:

-- | This module use a specialized line-unfolding parser.
-- See 'scan'.
type TextParser = P.Parsec ByteString ParserState

type ContentParser = ErrorT String
                            (RWS ParserState [String]
                                 (P.SourcePos, [Content]))


data ParserState = ParserState
    { psConv :: ByteString -> Text
    , psIConv :: ByteString -> CI Text
    }

instance Default ParserState where
    def = ParserState TE.decodeUtf8 (CI.mk . TE.decodeUtf8)

data Content = ContentLine SourcePos (CI Text) [(CI Text, [Text])] ByteString
             | Component SourcePos (CI Text) [Content]
               deriving (Show, Eq, Ord)

data UnwrapState = GoOn | SawR | SawN | SawRN deriving Eq

testICal :: IO (Either P.ParseError [Content])
testICal = do
    x <- B.readFile "example.ics"
    return $ runParser parseToContent def "-" x

testP :: Show a => ByteString -> TextParser a -> IO ()
testP b p = print $ runParser p def "-" b

-- {{{ Parse into content.

parseToContent :: TextParser [Content]
parseToContent = do content <- sepEndBy1 contentline (char '\n')
                    f <- psIConv <$> getState
                    return $ componentalize f content


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
     -- | Nothing: Fail.
     -- Just Nothing: Done, don't use last char.
     -- Just (Just state): Continue, collecting char unless EOF.
     -> (s -> Maybe Char -> Maybe (Maybe s))
     -> TextParser ByteString
scan state f = go state mempty
  where go st buf = do
            _ <- many (try unfold)
            c <- lookAhead (Just <$> P.anyChar <|> Nothing <$ P.eof)
            case (c, f st c) of
                 (_, Nothing) -> mzero
                 (Just c', Just (Just st')) ->
                    anyToken *> go st' (buf <> Bu.char8 c')
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

toText :: ByteString -> TextParser Text
toText b = do f <- psConv <$> getState
              return $ f b

toIText :: ByteString -> TextParser (CI Text)
toIText b = do f <- psIConv <$> getState
               return $ f b

contentline :: TextParser Content
contentline = do pos <- getPosition
                 n <- name
                 ps <- many (char ';' >> param)
                 _ <- char ':'
                 val <- value
                 return $ ContentLine pos n ps val

name :: TextParser (CI Text)
name = do n <- takeWhile1 isName <?> "name"
          toIText n

param :: TextParser (CI Text, [Text])
param = do n <- name
           _ <- char '='
           vs <- sepBy1 paramValue (char ',')
           return (n, vs)

paramValue :: TextParser Text
paramValue = paramtext <|> quotedString

paramtext :: TextParser Text
paramtext = toText =<< takeWhile1 isSafe <?> "paramtext"

value :: TextParser ByteString
value = takeWhile1 isValue <?> "value"

quotedString :: TextParser Text
quotedString = (do
    _ <- char '"'
    s <- takeWhile1 isQSafe
    _ <- char '"'
    toText s) <?> "quoted string"

-- }}}
-- {{{ Parse content lines into iCalendar components.

runCP :: ParserState -> ContentParser a
      -> (Either String a, (P.SourcePos, [Content]), [String])
runCP s = ((flip .) . flip) runRWS s undefined . runErrorT

down :: Content -> ContentParser a -> ContentParser a
down (Component p _ x) = down' (p, x)
down x@(ContentLine p _ _ _) = down' (p, [x])

down' :: (P.SourcePos, [Content]) -> ContentParser a -> ContentParser a
down' x m = get >>= \old -> put x >> m >>= (<$ put old)

-- return . runCP def . parseVCalendar . head . (\(Right x)->x) =<< testICal
-- | Parse a VCALENDAR component. 3.4
parseVCalendar :: Content -> ContentParser VCalendar
parseVCalendar c@(Component _ "VCALENDAR" _) = down c $ do
    vcProdId <- reqLine1 "PRODID" (parseSimple ProdId)
    vcVersion <- reqLine1 "VERSION" parseVersion
    vcScale <- optLine1 "CALSCALE" (parseSimpleI Scale)
    vcMethod <- optLine1 "METHOD" (parseSimpleI ((Just .) . Method))
    let vcOther = def -- tmp.
    vcTimeZones <- optCompN "VTIMEZONE" parseVTimeZone
    return VCalendar {..}
parseVCalendar _ = throwError "parseVCalendar: Content given not a VCALENDAR\
                              \ component."

-- | Parse a VTIMEZONE component. 3.6.5
parseVTimeZone :: Content -> ContentParser VTimeZone
parseVTimeZone c@(Component _ "VTIMEZONE" _) = down c $ do
    vtzId <- reqLine1 "TZID" parseTZID
    vtzLastMod <- optLine1 "LAST-MODIFIED" (Just .: parseLastModified)
    vtzUrl <- optLine1 "TZURL" (Just .: parseSimpleURI TZUrl)
    vtzStandardC <- optCompN "STANDARD" parseTZProp
    vtzDaylightC <- optCompN "DAYLIGHT" parseTZProp
    when (S.size vtzStandardC + S.size vtzDaylightC < 1) .
        throwError $ "VTIMEZONE must include at least one of the STANDARD or \
                     \DAYLIGHT components."
    return VTimeZone {..}
parseVTimeZone x = throwError $ "parseVTimeZone: " ++ show x

-- | Parse a VERSION property 3.7.4
parseVersion :: Content -> ContentParser ICalVersion
parseVersion (ContentLine _ "VERSION" o bs) = do
    c <- asks psConv
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
    tzidValue <- asks $ ($ bs) . psConv
    let tzidGlobal = (fst <$> T.uncons tzidValue) == Just '/'
        tzidOther = toO o
    return TZID {..}
parseTZID x = throwError $ "parseTZID: " ++ show x

parseTZProp :: Content -> ContentParser TZProp
parseTZProp c@(Component _ _ _) = down c $ do
    tzpDTStart <- reqLine1 "DTSTART" $
                    parseSimpleDateOrDateTime DTStartDateTime DTStartDate
    tzpTZOffsetTo <- reqLine1 "TZOFFSETTO" parseUTCOffset
    tzpTZOffsetFrom <- reqLine1 "TZOFFSETFROM" parseUTCOffset
    tzpRRule <- optLineN "RRULE" (parseRRule True tzpDTStart)
    tzpComment <- optLineN "COMMENT" (parseAltRepLang Comment)
    return TZProp {..}

-- | Parse RRule.
parseRRule :: Bool -- ^ If it's inside a TZProp
           -> DTStart
           -> Content -> ContentParser RRule
parseRRule isInTz dts (ContentLine _ "RRULE" o bs) = do
    case runParser (parseRecur isInTz dts) def "RRULE" bs of
         Left e -> throwError $ show e
         Right x -> do y <- x
                       return . RRule y $ toO o

-- | Parse Recur-value.
-- Partly implemented in parsec.
parseRecur :: Bool -- ^ If it's inside a TZProp
           -> DTStart -> TextParser (ContentParser Recur)
parseRecur isInTz dts =
    permute $ mkRecur <$$> (freq <* term)
                      <|?> (Right 1, untilCount <* term)
                      <|?> (1, istring "INTERVAL=" *> digits)
                      <|?> ([], istring "BYSECOND=" *> digitsN)
                      <|?> ([], istring "BYMINUTE=" *> digitsN)
                      <|?> ([], istring "BYHOUR=" *> digitsN)
                      <|?> ([], istring "BYWDAY=" *> sepBy wday (P.char ','))
                      <|?> ([], istring "BYMONTHDAY=" *> onum)
                      <|?> ([], istring "BYYEARDAY=" *> onum)
                      <|?> ([], istring "BYWEEKNO=" *> onum)
                      <|?> ([], istring "BYMONTH=" *> digitsN)
                      <|?> ([], istring "BYSETPOS=" *> onum)
                      <|?> (Monday, istring "WKST=" *> weekday)

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
        neg = maybe id (\x -> if x == '-' then negate else id)
                    <$> optional (P.oneOf "+-")
        wday = Right <$> weekday
           <|> (Left .) . (,) <$> (neg <*> digits) <*> weekday
        onum = sepBy1 (neg <*> digits) (P.char ',')
        untilCount = istring "UNTIL=" *> until'
                 <|> istring "COUNT=" *> (Right <$> digits)
        until' = do txt <- manyTill P.anyChar (void (P.char ';') <|> P.eof)
                    return . Left $
                        case dts of
                             DTStartDateTime _ _ ->
                                 Right <$> parseDateTime Nothing (B.pack txt)
                             DTStartDate _ _ ->
                                 Left <$> parseDate (B.pack txt)
        digits = foldl1 ((+).(*10)) . map digitToInt <$> many1 P.digit
        digitsN = sepBy1 digits (P.char ',')
        term = optional (P.char ';')
        istring = try . sequence . (map (\c -> P.char c <|> P.char (toLower c)))

        -- So I heard you like parsers...
        mkRecur f uc i s m h d md yd wn mo sp wkst = do
            uc' <- case uc of
                        Left x -> Left <$> x
                        Right y -> return (Right y)
            return $ Recur f uc' i s m h d md yd wn mo sp wkst

-- | Parse Last Modified, 3.8.7.3
parseLastModified :: Content -> ContentParser LastModified
parseLastModified (ContentLine _ "LAST-MODIFIED" o bs) = do
    lastModifiedValue <- mustBeUTC =<< parseDateTime Nothing bs
    let lastModifiedOther = toO o
    return LastModified {..}
parseLastModified x = throwError $ "parseLastModified: " ++ show x

-- | Parse a string to a Day. 3.3.4
parseDateStr :: String -> Maybe (Day, String)
parseDateStr = lastToMaybe . Time.readsTime L.defaultTimeLocale "%Y%m%d"

-- | Parse a string to a TimeOfDay, and a bool if it's in UTC.
parseTimeStr :: String -> Maybe (TimeOfDay, Bool)
parseTimeStr s = second (=="Z")
           <$> lastToMaybe (Time.readsTime L.defaultTimeLocale "%H%M%S" s)

-- | Parse a Date value. 3.3.4
parseDate :: ByteString -> ContentParser Date
parseDate bs = do
    str <- asks $ T.unpack . ($ bs) . psConv
    let dayRes = parseDateStr str
        Just (day, rest) = dayRes
    when (isNothing dayRes) .
        throwError $ "parseDate: " ++ str
    when (not $ null rest) $
        tell ["parseDate: extra content: " ++ rest]
    return $ Date day

-- | Parse a DateTime value. 3.3.5
parseDateTime :: Maybe Text -> ByteString -> ContentParser DateTime
parseDateTime mTZ bs = do
    str <- asks $ T.unpack . ($ bs) . psConv
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

-- | Parse a UTC Offset value. 3.3.14, 3.8.3.4, and 3.8.3.3
parseUTCOffset :: Content -> ContentParser UTCOffset
parseUTCOffset (ContentLine _ _ o bs) = do
    let str = B.unpack bs
        (s:rest) = str
        (t1:t2:m1:m2:sec) = map digitToInt rest
        (s1:s2:xs) = sec
        sign x = if s == '-' then 0 - x else x
    when (length str < 5 || or (map (not . isDigit) rest) || s `notElem` "+-"
                         || length sec `notElem` [0,2]) .
        throwError $ "parseUTCOffset: " ++ str
    return . UTCOffset (sign $ ((t1 * 10 + t2) * 60 + (m1 * 10 + m2)) * 60 +
                                if not (null sec) then s1 * 10 + s2 else 0)
                       $ toO o



-- | Convert a 'DateTime' to 'UTCTime', giving an appropriate error.
mustBeUTC :: DateTime -> ContentParser UTCTime
mustBeUTC (UTCDateTime x) = return x
mustBeUTC _ = throwError "DateTime-value must be UTC"

-- | Parse something simple with only a Text-field for the content, and
-- 'OtherParams'.
parseSimple :: (Text -> OtherParams -> b) -> Content -> ContentParser b
parseSimple k (ContentLine _ _ o bs) = do c <- asks psConv
                                          return $ k (c bs) (toO o)
parseSimple _ x = throwError $ "parseSimple: " ++ show x

-- | Parse something simple with only a CI Text-field for the content, and
-- 'OtherParams'.
parseSimpleI :: (CI Text -> OtherParams -> b) -> Content -> ContentParser b
parseSimpleI k (ContentLine _ _ o bs) = do c <- asks psIConv
                                           return $ k (c bs) (toO o)
parseSimpleI _ x = throwError $ "parseSimpleI: " ++ show x

-- | Parse something text with alternative representations, language
-- specification, and 'OtherParams'.
parseAltRepLang :: (Text -> Maybe URI.URI -> Maybe Language -> OtherParams -> a)
                -> Content -> ContentParser a
parseAltRepLang f (ContentLine _ _ o bs) = do
    conv <- asks psConv
    uri' <- mapM paramOnlyOne $ T.unpack .: lookup "ALTREP" o
    lang <- mapM paramOnlyOne $ CI.mk .: lookup "LANGUAGE" o
    let uri = URI.parseURI =<< uri'
    let o' = filter (\(x, _) -> x `notElem` ["ALTREP", "LANGUAGE"]) o
    when (isJust uri' && isNothing uri) .
        throwError $ "Invalid URI: " ++ fromJust uri'
    return $ f (conv bs) uri lang (toO o')

-- | Parse something simple with only a URI-field for the content, and
-- 'OtherParams'.
parseSimpleURI :: (URI.URI -> OtherParams -> a) -> Content -> ContentParser a
parseSimpleURI f (ContentLine _ _ o bs) = do
    uri <- asks (URI.parseURI . T.unpack . ($ bs) . psConv)
    when (isNothing uri) .
        throwError $ "Invalid URI: " ++ show bs
    return . f (fromJust uri) $ toO o
parseSimpleURI _ x = throwError $ "parseSimpleURI: " ++ show x

-- | Parse something which has either a 'Date' or a 'DateTime' value, and
-- 'OtherParams'. Uses DateTime if there is no vale parameter.
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
                           return . dt x $ toO o
         "DATE" -> do x <- parseDate bs
                      return . d x $ toO o

-- | Convert a property dictionary to 'OtherParams'.
toO :: [(CI Text, [Text])] -> OtherParams
toO = OtherParams . S.fromList . map (uncurry OtherParam)

-- {{{ Combinators
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

-- | One required ...
req1 :: CI Text -> (Content -> ContentParser b) -> ([Content], [Content])
     -> ContentParser b
req1 _ f ([x], xs) = modify (second $ const xs) >> f x
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

-- | Only allow one parameter value.
paramOnlyOne :: [a] -> ContentParser a
paramOnlyOne [x] = return x
paramOnlyOne _ = throwError $ "Only one parameter value allowed."

-- | Line predicate.
isLineNamed :: Content -> CI Text -> Bool
isLineNamed (ContentLine _ name _ _) n | name == n = True
isLineNamed _ _ = False

-- | Component predicate.
isComponentNamed :: Content -> CI Text -> Bool
isComponentNamed (Component _ name _) n | name == n = True
isComponentNamed _ _ = False
-- }}}
-- }}}
-- {{{ Util

lastToMaybe :: [a] -> Maybe a
lastToMaybe x = if null x then Nothing else Just $ last x
-- }}}
