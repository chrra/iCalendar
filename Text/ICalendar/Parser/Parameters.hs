{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.ICalendar.Parser.Parameters where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.RWS            (MonadWriter (tell))
import           Data.ByteString.Lazy.Char8   (ByteString)
import qualified Data.ByteString.Lazy.Char8   as B
import           Data.CaseInsensitive         (CI)
import           Data.Char
import           Data.Default
import           Data.Maybe
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as T

import           Codec.MIME.Parse             (parseMIMEType)
import           Codec.MIME.Type              (MIMEType, mimeType)
import qualified Text.Parsec                  as P
import           Text.Parsec.Combinator       hiding (optional)
import           Text.Parsec.Perm
import           Text.Parsec.Prim             hiding ((<|>))

import           Text.ICalendar.Parser.Common
import           Text.ICalendar.Types

parseRelated :: CI Text -> ContentParser Related
parseRelated "START" = return Start
parseRelated "END"   = return End
parseRelated x       = throwError $ "parseRelated: " ++ show x

-- | Parse relationship type. 3.2.15
parseRelType :: CI Text -> RelType
parseRelType "PARENT"  = Parent
parseRelType "CHILD"   = Child
parseRelType "SIBLING" = Sibling
parseRelType x         = RelTypeX x

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
parseMime t = let m = mimeType .: parseMIMEType $ T.toStrict t
               in maybe (throwError $ "parseMime: " ++ show t) return m

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



parseUTCPeriod :: ByteString -> ContentParser UTCPeriod
parseUTCPeriod bs = do
    let (dateTime', x) = B.drop 1 <$> B.break (=='/') bs
    when (B.null x) . throwError $ "Invalid UTCperiod: " ++ show bs
    dateTime <- mustBeUTC =<< parseDateTime Nothing dateTime'
    case B.head x of
         z | z `elem` ("+-P"::String) -> UTCPeriodDuration dateTime
                                    <$> parseDuration "period" x
         _ -> UTCPeriodDates dateTime <$> (mustBeUTC =<< parseDateTime Nothing x)

parsePeriod :: Maybe Text -> ByteString -> ContentParser Period
parsePeriod tzid bs = do
    let (dateTime', x) = B.drop 1 <$> B.break (=='/') bs
    when (B.null x) . throwError $ "Invalid period: " ++ show bs
    dateTime <- parseDateTime tzid dateTime'
    case B.head x of
         z | z `elem` ("+-P"::String) -> PeriodDuration dateTime
                                    <$> parseDuration "period" x
         _ -> PeriodDates dateTime <$> parseDateTime tzid x
