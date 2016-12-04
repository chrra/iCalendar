{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.ICalendar.Values.Internal where

import           Control.Monad.Except               (runExceptT, throwError)
import           Control.Monad.RWS                  (MonadReader (..),
                                                     MonadState (..),
                                                     MonadWriter (..), evalRWS)
import           Data.ByteString.Lazy               (ByteString)
import           Data.CaseInsensitive               (CI)
import           Data.Default
import           Data.List                          (partition)
import           Data.Monoid                        ((<>))
import           Data.Text.Lazy                     (Text)

import           Text.ICalendar.Internal
import           Text.ICalendar.Parameters
import           Text.ICalendar.Parameters.Internal


class ICalendarValue a where
    valueType             :: a -> ValueType
    valueParser           :: ValueParser a
    valuePrinter          :: a -> ValuePrinter
    valueParameterPrinter :: a -> ParameterPrinter

type ValueParser = ICalendarParser ([(CI Text, [Text])], ByteString)

newtype ValuePrinter = ValuePrinter
    { runValuePrinter ::
        forall x. Monoid x =>
            (ByteString -> x) -> x
    }

instance Monoid ValuePrinter where
    mempty = ValuePrinter (const mempty)
    (ValuePrinter f) `mappend` (ValuePrinter g) =
        ValuePrinter (\x -> f x <> g x)

runValueParser :: ValueParser a
               -> Line
               -> [(CI Text, [Text])]
               -> ByteString
               -> (Either String a, [String])
runValueParser p l pr v = evalRWS (runExceptT p) l (pr, v)

getValue :: ValueParser ByteString
getValue = snd <$> get

getParamsNamed :: CI Text -> ValueParser [(CI Text, [Text])]
getParamsNamed n = state $ \(s,v) -> let (x, y) = partition ((==n).fst) s
                                      in (x, (y, v))

runParamParser :: ICalendarParameter a => [Text] -> ValueParser a
runParamParser pvs = do
    line <- ask
    let (y, w) = runParameterParser parameterParser line pvs
    case y of
        Left e  -> throwError e
        Right p -> do tell w
                      return p

-- | Parses none or one parameter.
optParam1 :: forall a. ICalendarParameter a => ValueParser (Maybe a)
optParam1 = do
    x <- getParamsNamed name
    case x of
        [] ->
            return Nothing
        [(_, pvs)] ->
            Just <$> runParamParser pvs
        _  ->
            pErr $ "too many parameters " ++ show name
  where
    name = parameterName (undefined :: a)

-- | Parses one required parameter.
reqParam1 :: forall a. ICalendarParameter a => ValueParser a
reqParam1 = do
    x <- getParamsNamed name
    case x of
        [] ->
            pErr $ "missing parameter " ++ show name
        [(_, pvs)] ->
            runParamParser pvs
        _  ->
            pErr $ "too many parameters " ++ show name
  where
    name = parameterName (undefined :: a)

-- | Parses one parameter, or gives the default if it's not present.
defParam1 :: forall a. (Default a, ICalendarParameter a)
          => ValueParser a
defParam1 = do
    x <- getParamsNamed name
    case x of
        [] ->
            return def
        [(_, pvs)] ->
            runParamParser pvs
        _  ->
            pErr $ "too many parameters " ++ show name
  where
    name = parameterName (undefined :: a)
