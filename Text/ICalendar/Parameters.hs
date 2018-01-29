{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
-- | Property parameters.
--
-- <https://tools.ietf.org/html/rfc5545#section-3.2>
module Text.ICalendar.Parameters
    (
      ParameterPrinter
    , ParameterParser
    , ICalendarParameter(..)

    , AltRep(..)
    , CN(..)
    , CUType(..)
    , DelegatedFrom(..)
    , DelegatedTo(..)
    , Dir(..)
    , Encoding(..)
    , FMTType(..)
    , FBType(..)
    , Language(..)
    , Member(..)
    , PartStatEvent(..)
    , PartStatTodo(..)
    , PartStatJournal(..)
    , Range(..)
    , Related(..)
    , RelType(..)
    , Role(..)
    , RSVP(..)
    , SentBy(..)
    , ValueType(..)
    , TZID(..)
    , OtherParameter(..)
    , OtherParameters(..)
    , printOtherParameters
    ) where

import           Codec.MIME.Parse                   (parseMIMEType)
import           Codec.MIME.Type                    (MIMEType, mimeType,
                                                     showMIMEType)
import           Control.Monad.Except
import           Control.Monad.RWS
import           Data.Attoparsec.Combinator         (many1)
import           Data.CaseInsensitive               (CI)
import qualified Data.CaseInsensitive               as CI
import           Data.Default                       (Default (..))
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.String                        (IsString (..))
import qualified Data.Text                          as TS
import           Data.Text.Lazy                     (Text)
import qualified Data.Text.Lazy                     as T
import           Data.Typeable                      (Typeable)
import           GHC.Generics                       (Generic)
import           Network.URI                        (URI)
import qualified Network.URI                        as URI

import           Text.ICalendar.Internal
import           Text.ICalendar.Parameters.Internal


-- | Note: the parser doesn't backtrack, so if you fail you have to undo any
-- state changes.
getVal :: ParameterParser Text
getVal = do
        x <- state $ splitAt 1
        when (null x) $
            pErr "missing parameter value"
        return $ head x

putVal :: Text -> ParameterParser ()
putVal a = state (\xs -> ((), a:xs))

paramDone :: ParameterParser ()
paramDone = do
    x <- get
    unless (null x) $
        pErr "remainding parameter values"

parseUriParam :: ParameterParser URI
parseUriParam = do
    s <- getVal
    case URI.parseURI $ T.unpack s of
        Just x  -> return x
        Nothing -> do putVal s
                      pErr $ "Invalid URI: " ++ show s

parseUriSet :: ParameterParser (Set URI)
parseUriSet = Set.fromList <$> many1 parseUriParam <* paramDone

-- | Default parameter printer.
pp :: ICalendarParameter a
   => (a -> [(Quoting, Text)])
   -> a
   -> ParameterPrinter
pp g x = ParameterPrinter (\f -> f (parameterName x) (g x))

-- | Default parameter printer for parameters with a default that can be elided.
defPP :: (ICalendarParameter a, Default a, Eq a)
   => (a -> [(Quoting, Text)])
   -> a
   -> ParameterPrinter
defPP g x =
    if x == def
       then mempty
       else ParameterPrinter (\f -> f (parameterName x) (g x))

printUri :: URI -> (Quoting, Text)
printUri u = (NeedQuotes, T.pack $ show u)

-- | Alternate representation, 3.2.1.
newtype AltRep = AltRep { altRepValue :: URI}
                 deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter AltRep where
    parameterName _  = "ALTREP"
    parameterParser  = AltRep <$> parseUriParam <* paramDone
    parameterPrinter = pp $ \(AltRep u) -> [printUri u]

-- | Common name, 3.2.2.
newtype CN = CN { cnValue :: Text }
             deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter CN where
    parameterName _  = "CN"
    parameterParser  = CN <$> getVal <* paramDone
    parameterPrinter = pp $ \(CN u) -> [(NeedQuotes, u)]

-- | Calendar User Type, 3.2.3.
--
-- Unrecognized CUTypeX MUST be treated as Unknown.
data CUType
    = Individual
    | Group
    | Resource
    | Room
    | Unknown
    | CUTypeX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter CUType where
    parameterName _  = "CUTYPE"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "INDIVIDUAL" = Individual
        f "GROUP"      = Group
        f "RESOURCE"   = Resource
        f "ROOM"       = Room
        f "UNKNOWN"    = Unknown
        f x            = CUTypeX x
    parameterPrinter = defPP f
      where
        f Individual  = [(NoQuotes, "INDIVIDUAL")]
        f Group       = [(NoQuotes, "GROUP")]
        f Resource    = [(NoQuotes, "RESOURCE")]
        f Room        = [(NoQuotes, "ROOM")]
        f Unknown     = [(NoQuotes, "UNKNOWN")]
        f (CUTypeX x) = [(Optional, CI.original x)]

-- | 'Individual'.
instance Default CUType where
    def = Individual

-- | Delegated from, 3.2.4.
--
-- Can be set on CalAddress.
newtype DelegatedFrom = DelegatedFrom { delegatedFromValue :: Set URI }
                        deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter DelegatedFrom where
    parameterName _  = "DELEGATED-FROM"
    parameterParser  = DelegatedFrom <$> parseUriSet
    parameterPrinter p@(DelegatedFrom x) =
        if Set.null x
           then mempty
           else ParameterPrinter g
      where
        g h = h (parameterName p) (printUri <$> Set.toList x)

-- | Delegated to, 3.2.5.
--
-- Can be set on CalAddress.
newtype DelegatedTo = DelegatedTo { delegatedToValue :: Set URI }
                      deriving (Show, Eq, Ord, Typeable, Generic, Default)

instance ICalendarParameter DelegatedTo where
    parameterName _  = "DELEGATED-TO"
    parameterParser  = DelegatedTo <$> parseUriSet
    parameterPrinter p@(DelegatedTo x) =
        if Set.null x
           then mempty
           else ParameterPrinter g
      where
        g h = h (parameterName p) (printUri <$> Set.toList x)

-- | Directory entry reference, 3.2.6.
newtype Dir = Dir { dirValue :: URI }
              deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter Dir where
    parameterName _  = "DIR"
    parameterParser  = Dir <$> parseUriParam <* paramDone
    parameterPrinter = pp ((:[]) . printUri . dirValue)

-- | Inline encoding, 3.2.7.
data Encoding = EightBit | Base64
                deriving (Show, Eq, Ord, Typeable, Generic)

instance Default Encoding where
    def = EightBit

instance ICalendarParameter Encoding where
    parameterName _  = "ENCODING"
    parameterParser  = f . CI.mk =<< getVal <* paramDone
      where
        f "8BIT"   = return EightBit
        f "BASE64" = return Base64
        f x        = pErr $ "parameterParser{Encoding}: unknown encoding, " ++
                            show x
    parameterPrinter = defPP f
      where
        f EightBit = [(NoQuotes, "8BIT")]
        f Base64   = [(NoQuotes, "BASE64")]

-- | Format Type, 3.2.8
newtype FMTType = FMTType { fmtTypeValue :: MIMEType }
                  deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter FMTType where
    parameterName _  = "FMTTYPE"
    parameterParser  = f =<< getVal <* paramDone
      where
        f x = maybe (pErr $ "parameterParser{FMTType}: bad " ++ show x)
                    (return . FMTType . mimeType)
                    (parseMIMEType $ T.toStrict x)
    parameterPrinter = pp f
      where
        f (FMTType m) = [(Optional, T.fromStrict $ showMIMEType m)]

instance IsString FMTType where
    fromString = maybe (error "bad mimetype") (FMTType . mimeType)
               . parseMIMEType . TS.pack

-- | Free/Busy Time Type. 3.2.9.
--
-- Unrecognized FBTypeX MUST be treated as Busy.
data FBType
    = Free
    | Busy
    | BusyUnavailable
    | BusyTentative
    | FBTypeX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter FBType where
    parameterName _  = "FBTYPE"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "FREE"             = Free
        f "BUSY"             = Busy
        f "BUSY-UNAVAILABLE" = BusyUnavailable
        f "BUSY-TENTATIVE"   = BusyTentative
        f x                  = FBTypeX x
    parameterPrinter = defPP f
      where
        f Free            = [(NoQuotes, "FREE")]
        f Busy            = [(NoQuotes, "BUSY")]
        f BusyUnavailable = [(NoQuotes, "BUSY-UNAVAILABLE")]
        f BusyTentative   = [(NoQuotes, "BUSY-TENTATIVE")]
        f (FBTypeX x)     = [(Optional, CI.original x)]

-- | 'Busy'
instance Default FBType where
    def = Busy

-- | Language, 3.2.10.
newtype Language = Language (CI Text) -- TODO: RFC5646 types and parser.
                   deriving (Eq, Show, Ord, Typeable, Generic)

instance ICalendarParameter Language where
    parameterName _  = "LANGUAGE"
    parameterParser  = Language . CI.mk <$> getVal <* paramDone
    parameterPrinter = pp $ \(Language l) -> [(Optional, CI.original l)]

-- | Member, 3.2.11.
newtype Member = Member (Set URI)
                 deriving (Eq, Show, Ord, Typeable, Generic)

instance ICalendarParameter Member where
    parameterName _  = "MEMBER"
    parameterParser  = Member <$> parseUriSet
    parameterPrinter p@(Member x) =
        if Set.null x
           then mempty
           else ParameterPrinter g
      where
        g h = h (parameterName p) (printUri <$> Set.toList x)

-- | Participation Status for VEVENT. 3.2.12.
data PartStatEvent
    = EventNeedsAction
    | EventAccepted
    | EventDeclined
    | EventTentative
    | EventDelegated
    | EventPartStatX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter PartStatEvent where
    parameterName _  = "PARTSTAT"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "NEEDS-ACTION" = EventNeedsAction
        f "ACCEPTED"     = EventAccepted
        f "DECLINED"     = EventDeclined
        f "TENTATIVE"    = EventTentative
        f "DELEGATED"    = EventDelegated
        f x              = EventPartStatX x
    parameterPrinter = defPP f
      where
        f EventNeedsAction   = [(NoQuotes, "NEEDS-ACTION")]
        f EventAccepted      = [(NoQuotes, "ACCEPTED")]
        f EventDeclined      = [(NoQuotes, "DECLINED")]
        f EventTentative     = [(NoQuotes, "TENTATIVE")]
        f EventDelegated     = [(NoQuotes, "DELEGATED")]
        f (EventPartStatX x) = [(Optional, CI.original x)]

-- | 'EventNeedsAction'
instance Default PartStatEvent where
    def = EventNeedsAction

-- | Participation Status for VTODO. 3.2.12.
data PartStatTodo
    = PartStatTodoNeedsAction
    | TodoAccepted
    | TodoDeclined
    | TodoTentative
    | TodoDelegated
    | TodoCompleted
    | TodoInProcess
    | TodoPartStatX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter PartStatTodo where
    parameterName _  = "PARTSTAT"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "NEEDS-ACTION" = PartStatTodoNeedsAction
        f "ACCEPTED"     = TodoAccepted
        f "DECLINED"     = TodoDeclined
        f "TENTATIVE"    = TodoTentative
        f "DELEGATED"    = TodoDelegated
        f "COMPLETED"    = TodoCompleted
        f "IN-PROCESS"   = TodoInProcess
        f x              = TodoPartStatX x
    parameterPrinter = defPP f
      where
        f PartStatTodoNeedsAction = [(NoQuotes, "NEEDS-ACTION")]
        f TodoAccepted            = [(NoQuotes, "ACCEPTED")]
        f TodoDeclined            = [(NoQuotes, "DECLINED")]
        f TodoTentative           = [(NoQuotes, "TENTATIVE")]
        f TodoDelegated           = [(NoQuotes, "DELEGATED")]
        f TodoCompleted           = [(NoQuotes, "COMPLETED")]
        f TodoInProcess           = [(NoQuotes, "IN-PROCESS")]
        f (TodoPartStatX x)       = [(Optional, CI.original x)]

-- | 'PartStatTodoNeedsAction'
instance Default PartStatTodo where
    def = PartStatTodoNeedsAction

-- | Participation Status for VJOURNAL. 3.2.12.
data PartStatJournal
    = JournalNeedsAction
    | JournalAccepted
    | JournalDeclined
    | JournalPartStatX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter PartStatJournal where
    parameterName _  = "PARTSTAT"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "NEEDS-ACTION" = JournalNeedsAction
        f "ACCEPTED"     = JournalAccepted
        f "DECLINED"     = JournalDeclined
        f x              = JournalPartStatX x
    parameterPrinter = defPP f
      where
        f JournalNeedsAction   = [(NoQuotes, "NEEDS-ACTION")]
        f JournalAccepted      = [(NoQuotes, "ACCEPTED")]
        f JournalDeclined      = [(NoQuotes, "DECLINED")]
        f (JournalPartStatX x) = [(Optional, CI.original x)]

-- | 'JournalNeedsAction'
instance Default PartStatJournal where
    def = JournalNeedsAction

-- | Recurrence Identifier Range. 3.2.13.
data Range = ThisAndFuture | ThisAndPrior
             deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter Range where
    parameterName _  = "RANGE"
    parameterParser  = f . CI.mk =<< getVal <* paramDone
      where
        f "THISANDFUTURE" = return ThisAndFuture
        f "THISANDPRIOR"  = do tell ["THISANDPRIOIR RANGE is deprecated."]
                               return ThisAndPrior
        f x               = pErr $ "parameterParser{Range}: unknown range, " ++
                                   show x
    parameterPrinter x = ParameterPrinter $ \f ->
        if x == ThisAndPrior
           then mempty
           else f (parameterName x) [(NoQuotes, "THISANDFUTURE")]

-- | Alarm Trigger Relationship. 3.2.14.
data Related = Start | End
               deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter Related where
    parameterName _  = "RELATED"
    parameterParser  = f . CI.mk =<< getVal <* paramDone
      where
        f "START" = return Start
        f "END"   = return End
        f x       = pErr $ "parameterParser{Related}: unknown value, " ++ show x
    parameterPrinter = defPP f
      where
        f Start = [(NoQuotes, "START")]
        f End   = [(NoQuotes, "END")]

-- | 'Start'
instance Default Related where
    def = Start

-- | Relationship Type. 3.2.15.
--
-- Unrecognized RelTypeX values MUST be treated as Parent.
data RelType = Parent | Child | Sibling | RelTypeX (CI Text)
               deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter RelType where
    parameterName _  = "RELTYPE"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "PARENT"  = Parent
        f "CHILD"   = Child
        f "SIBLING" = Sibling
        f x         = RelTypeX x
    parameterPrinter = defPP f
      where
        f Parent       = [(NoQuotes, "PARENT")]
        f Child        = [(NoQuotes, "CHILD")]
        f Sibling      = [(NoQuotes, "SIBLING")]
        f (RelTypeX x) = [(Optional, CI.original x)]

-- | 'Parent'
instance Default RelType where
    def = Parent

-- | Role. 3.2.16.
--
-- Unrecognized RoleX values MUST be treated as ReqParticipant.
data Role = Chair
          | ReqParticipant
          | OptParticipant
          | NonParticipant
          | RoleX (CI Text)
            deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter Role where
    parameterName _  = "ROLE"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "CHAIR"           = Chair
        f "REQ-PARTICIPANT" = ReqParticipant
        f "OPT-PARTICIPANT" = OptParticipant
        f "NON-PARTICIPANT" = NonParticipant
        f x                 = RoleX x
    parameterPrinter = defPP f
      where
        f Chair          = [(NoQuotes, "CHAIR")]
        f ReqParticipant = [(NoQuotes, "REQ-PARTICIPANT")]
        f OptParticipant = [(NoQuotes, "OPT-PARTICIPANT")]
        f NonParticipant = [(NoQuotes, "NON-PARTICIPANT")]
        f (RoleX x)      = [(Optional, CI.original x)]

-- | 'ReqParticipant'.
instance Default Role where
    def = ReqParticipant

-- | RSVP expectation, 3.2.17.
newtype RSVP = RSVP Bool
               deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Generic)

instance ICalendarParameter RSVP where
    parameterName _  = "RSVP"
    parameterParser  = f . CI.mk =<< getVal <* paramDone
      where
        f "TRUE"  = return $ RSVP True
        f "FALSE" = return $ RSVP False
        f x       = pErr $ "parameterParser{RSVP}: Unknown Boolean " ++ show x
    parameterPrinter = defPP f
      where
        f (RSVP True)  = [(NoQuotes, "TRUE")]
        f (RSVP False) = [(NoQuotes, "FALSE")]

-- | False.
instance Default RSVP where
    def = RSVP False

-- | Sent by, 3.2.18.
--
-- Can be set on CalAddress.
newtype SentBy = SentBy { sentByValue :: URI }
                 deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter SentBy where
    parameterName _  = "SENT-BY"
    parameterParser  = SentBy <$> parseUriParam <* paramDone
    parameterPrinter = pp ((:[]) . printUri . sentByValue)

-- | Time Zone ID, 3.2.19
newtype TZID = TZID { tzidValue :: Text }
                    deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter TZID where
    parameterName _  = "TZID"
    parameterParser  = TZID <$> getVal <* paramDone
    parameterPrinter = pp (\x -> [(NoQuotes, tzidValue x)])

-- | Value Data Types, 3.2.20
data ValueType = ValueTypeBinary
               | ValueTypeBoolean
               | ValueTypeCalAddress
               | ValueTypeDate
               | ValueTypeDateTime
               | ValueTypeDuration
               | ValueTypeFloat
               | ValueTypeInteger
               | ValueTypePeriod
               | ValueTypeRecur
               | ValueTypeText
               | ValueTypeTime
               | ValueTypeUri
               | ValueTypeUtcOffset
               | ValueTypeX (CI Text)
                 deriving (Show, Eq, Ord, Typeable, Generic)

instance ICalendarParameter ValueType where
    parameterName _  = "VALUE"
    parameterParser  = f . CI.mk <$> getVal <* paramDone
      where
        f "BINARY"      = ValueTypeBinary
        f "BOOLEAN"     = ValueTypeBoolean
        f "CAL-ADDRESS" = ValueTypeCalAddress
        f "DATE"        = ValueTypeDate
        f "DATE-TIME"   = ValueTypeDateTime
        f "DURATION"    = ValueTypeDuration
        f "FLOAT"       = ValueTypeFloat
        f "INTEGER"     = ValueTypeInteger
        f "PERIOD"      = ValueTypePeriod
        f "RECUR"       = ValueTypeRecur
        f "TEXT"        = ValueTypeText
        f "TIME"        = ValueTypeTime
        f "URI"         = ValueTypeUri
        f "UTC-OFFSET"  = ValueTypeUtcOffset
        f x             = ValueTypeX x
    parameterPrinter = pp f
      where
        f ValueTypeBinary     = [(NoQuotes, "BINARY")]
        f ValueTypeBoolean    = [(NoQuotes, "BOOLEAN")]
        f ValueTypeCalAddress = [(NoQuotes, "CAL-ADDRESS")]
        f ValueTypeDate       = [(NoQuotes, "DATE")]
        f ValueTypeDateTime   = [(NoQuotes, "DATE-TIME")]
        f ValueTypeDuration   = [(NoQuotes, "DURATION")]
        f ValueTypeFloat      = [(NoQuotes, "FLOAT")]
        f ValueTypeInteger    = [(NoQuotes, "INTEGER")]
        f ValueTypePeriod     = [(NoQuotes, "PERIOD")]
        f ValueTypeRecur      = [(NoQuotes, "RECUR")]
        f ValueTypeText       = [(NoQuotes, "TEXT")]
        f ValueTypeTime       = [(NoQuotes, "TIME")]
        f ValueTypeUri        = [(NoQuotes, "URI")]
        f ValueTypeUtcOffset  = [(NoQuotes, "UTC-OFFSET")]
        f (ValueTypeX x)      = [(Optional, CI.original x)]

-- | One other parameter, either x-param or iana-param.
data OtherParameter
    = OtherParameter
    { otherParameterName   :: CI Text
    , otherParameterValues :: [Text]
    } deriving (Show, Eq, Ord, Typeable, Generic)

-- | Other parameters, either x-param or other iana-param.
newtype OtherParameters = OtherParameters (Set OtherParameter)
                          deriving (Show, Eq, Ord, Typeable, Generic, Default)

printOtherParameters :: OtherParameters -> ParameterPrinter
printOtherParameters (OtherParameters xs) = flip foldMap xs $ \p ->
    ParameterPrinter $ \f ->
        f (otherParameterName p)
          (map (Optional,) $ otherParameterValues p)
