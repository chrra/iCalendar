{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.ICalendar.Properties.Internal
    ( module Text.ICalendar.Properties.Internal
    -- shared between values and properties:
    , getParamsNamed
    , optParam1
    , reqParam1
    , defParam1
    ) where

import           Control.Monad.Except           (runExceptT)
import           Control.Monad.RWS              (MonadState (..), evalRWS)
import           Data.ByteString.Lazy           (ByteString)
import           Data.CaseInsensitive           (CI)
import           Data.Monoid                    ((<>))
import qualified Data.Set                       as Set
import           Data.Text.Lazy                 (Text)

import           Text.ICalendar.Internal
import           Text.ICalendar.Parameters
import           Text.ICalendar.Values
import           Text.ICalendar.Values.Internal


type PropertyParser = ICalendarParser ([(CI Text, [Text])], ByteString)

class ICalendarProperty a where
    propertyName         :: a -> CI Text
    propertyDefaultValue :: a -> ValueType
    propertyValue        :: a -> Value
    propertyParser       :: PropertyParser a
    propertyPrinter      :: a -> PropertyPrinter

newtype PropertyPrinter = PropertyPrinter
    { runPropertyPrinter ::
        forall x. Monoid x =>
            (CI Text -> ParameterPrinter -> ValuePrinter -> x) -> x
    }

instance Monoid PropertyPrinter where
    mempty = PropertyPrinter (const mempty)
    (PropertyPrinter f) `mappend` (PropertyPrinter g) =
        PropertyPrinter (\x -> f x <> g x)

runPropertyParser :: PropertyParser a
                  -> Line
                  -> [(CI Text, [Text])]
                  -> ByteString
                  -> (Either String a, [String])
runPropertyParser p l s bs = evalRWS (runExceptT p) l (s, bs)

-- | Dumps the remainding into 'OtherParameters'
parseOtherParameters :: PropertyParser OtherParameters
parseOtherParameters = do
    x <- state $ \(x, y) -> (x, ([], y))
    return . OtherParameters . Set.fromList . flip map x $
        uncurry OtherParameter
