{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.ICalendar.Components.Internal where

import           Control.Monad.Except               (runExceptT, throwError)
import           Control.Monad.RWS                  (MonadState (..),
                                                     MonadWriter (..), evalRWS)
import           Data.CaseInsensitive               (CI)
import           Data.List                          (partition)
import           Data.Monoid                        ((<>))
import           Data.Text.Lazy                     (Text)

import           Text.ICalendar.Internal
import           Text.ICalendar.Properties.Internal



class ICalendarComponent a where
    componentName    :: a -> CI Text
    componentParser  :: ComponentParser a
    componentPrinter :: a -> ComponentPrinter

type ComponentParser = ICalendarParser [Content]

newtype ComponentPrinter = ComponentPrinter
    { runComponentPrinter ::
        forall x. Monoid x =>
            (CI Text -> PropertyPrinter -> x) -> x
    }

instance Monoid ComponentPrinter where
    mempty = ComponentPrinter (const mempty)
    (ComponentPrinter f) `mappend` (ComponentPrinter g) =
        ComponentPrinter (\x -> f x <> g x)

runComponentParser :: ComponentParser a
                   -> Line
                   -> [Content]
                   -> (Either String a, [String])
runComponentParser = evalRWS . runExceptT

optProp1 :: forall a. ICalendarProperty a => ComponentParser (Maybe a)
optProp1 = do
    x <- state . partition $ contentLineNamed name
    case x of
        [] ->
            return Nothing
        [ContentLine line _ ps v] -> do
            let (y, w) = evalRWS (runExceptT propertyParser) line (ps, v)
            case y of
                Left e  -> throwError e
                Right p -> do tell w
                              return (Just p)
        _  ->
            pErr $ "too many properties " ++ show name
  where
    name = propertyName (undefined :: a)
