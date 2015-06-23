{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- | iCalendar parameter types.
--
-- <https://tools.ietf.org/html/rfc5545#section-3.2>
module Text.ICalendar.Types.Parameters
    (
      AltRep(..)
    , CN(..)
    , CUType(..)
    , DelegatedFrom(..)
    , DelegatedTo(..)
    , Dir(..)
    , FBType(..)
    -- | Omitted:
    --
    -- Encoding embedded directly in property 'Attachment'
    --
    -- @
    --     FMTType = 'Maybe' 'MIMEType'
    -- @
    , Language(..)
    , Member(..)
    , PartStat(..)
    , Range(..)
    , Related(..)
    , RelType(..)
    , Role(..)
    , RSVP(..)
    , SentBy(..)
    -- | Omitted:
    --
    -- TZID specified as Text directly in 'DateTime'
    --
    -- Value encoded directly in the various properties.
    , OtherParam(..)
    , OtherParams(..)
    ) where

import           Data.CaseInsensitive       (CI)
import           Data.Default               (Default (..))
import           Data.Set                   (Set)
import           Data.Text.Lazy             (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Network.URI                (URI)

-- | Alternate representation, 3.2.1.
newtype AltRep = AltRep URI
                 deriving (Show, Eq, Ord, Typeable, Generic)

-- | Common name, 3.2.2.
newtype CN = CN Text
             deriving (Show, Eq, Ord, Typeable, Generic)

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

-- | 'Individual'.
instance Default CUType where
    def = Individual

-- | Delegated from, 3.2.4.
--
-- Can be set on CalAddress.
newtype DelegatedFrom = DelegatedFrom (Set URI)
                        deriving (Show, Eq, Ord, Typeable, Generic)

-- | Delegated to, 3.2.5.
--
-- Can be set on CalAddress.
newtype DelegatedTo = DelegatedTo (Set URI)
                      deriving (Show, Eq, Ord, Typeable, Generic)

-- | Directory entry reference, 3.2.6.
newtype Dir = Dir URI
              deriving (Show, Eq, Ord, Typeable, Generic)

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

-- | 'Busy'
instance Default FBType where
    def = Busy

-- | Language, 3.2.10.
newtype Language = Language (CI Text) -- TODO: RFC5646 types and parser.
                   deriving (Eq, Show, Ord, Typeable, Generic)

-- | Member, 3.2.11.
newtype Member = Member (Set URI)
                 deriving (Eq, Show, Ord, Typeable, Generic)

-- | Participation Status. 3.2.12.
data PartStat -- Splitting requires splitting attendee too...
    = PartStatNeedsAction
    | Accepted
    | Declined
    | Tentative
    | Delegated
    | PartStatCompleted
    | InProcess
    | PartStatX (CI Text)
      deriving (Show, Eq, Ord, Typeable, Generic)

-- | 'PartStatNeedsAction'
instance Default PartStat where
    def = PartStatNeedsAction

-- | Recurrence Identifier Range. 3.2.13.
data Range = ThisAndFuture | ThisAndPrior
             deriving (Show, Eq, Ord, Typeable, Generic)

-- | Alarm Trigger Relationship. 3.2.14.
data Related = Start | End
               deriving (Show, Eq, Ord, Typeable, Generic)

-- | 'Start'
instance Default Related where
    def = Start

-- | Relationship Type. 3.2.15.
--
-- Unrecognized RelTypeX values MUST be treated as Parent.
data RelType = Parent | Child | Sibling | RelTypeX (CI Text)
               deriving (Show, Eq, Ord, Typeable, Generic)

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

-- | 'ReqParticipant'.
instance Default Role where
    def = ReqParticipant

-- | RSVP expectation, 3.2.17.
newtype RSVP = RSVP Bool
               deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Generic)

-- | False.
instance Default RSVP where
    def = RSVP False

-- | Sent by, 3.2.18.
--
-- Can be set on CalAddress.
newtype SentBy = SentBy URI
                 deriving (Show, Eq, Ord, Typeable, Generic)

-- | One other parameter, either x-param or iana-param.
data OtherParam = OtherParam (CI Text) [Text]
                  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Other parameters, either x-param or other iana-param.
data OtherParams = OtherParams (Set OtherParam)
                   deriving (Show, Eq, Ord, Typeable, Generic)

-- | Empty.
instance Default OtherParams where
    def = OtherParams def
