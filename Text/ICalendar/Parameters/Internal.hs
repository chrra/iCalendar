{-# LANGUAGE RankNTypes #-}
module Text.ICalendar.Parameters.Internal where

import           Control.Monad.Except    (runExceptT)
import           Control.Monad.RWS       (evalRWS)
import           Data.CaseInsensitive    (CI)
import           Data.Monoid             ((<>))
import           Data.Text.Lazy          (Text)

import           Text.ICalendar.Internal


type ParameterParser = ICalendarParser [Text]

data Quoting = NeedQuotes | Optional | NoQuotes
               deriving (Eq, Ord, Show)

newtype ParameterPrinter = ParameterPrinter
    { runParameterPrinter ::
        forall x. Monoid x =>
            (CI Text -> [(Quoting, Text)] -> x) -> x
    }

runParameterParser :: ParameterParser a
                   -> Line
                   -> [Text]
                   -> (Either String a, [String])
runParameterParser = evalRWS . runExceptT

instance Monoid ParameterPrinter where
    mempty = ParameterPrinter (const mempty)
    (ParameterPrinter f) `mappend` (ParameterPrinter g) =
        ParameterPrinter (\x -> f x <> g x)

class ICalendarParameter a where
    parameterName    :: a -> CI Text
    parameterParser  :: ParameterParser a
    parameterPrinter :: a -> ParameterPrinter
