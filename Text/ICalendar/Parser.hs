{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.ICalendar.Parser
    ( parseICalendar
    , parseICalendarFile
    , parseICal
    , parseICalFile
    , DecodingFunctions(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.RWS          (runRWS)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Monoid
import           Prelude

import Text.Parsec.ByteString.Lazy ()
import Text.Parsec.Pos
import Text.Parsec.Prim            hiding (many, (<|>))
import Text.Parsec.Text.Lazy       ()

import Text.ICalendar.Parser.Common
import Text.ICalendar.Parser.Components
import Text.ICalendar.Parser.Content
import Text.ICalendar.Types


-- | Parse a ByteString containing iCalendar data.
--
-- Returns either an error, or a tuple of the result and a list of warnings.
parseICalendar :: DecodingFunctions
               -> FilePath -- ^ Used in error messages.
               -> ByteString
               -> Either String ([VCalendar], [String])
parseICalendar s f bs = do
    a <- either (Left . show) Right $ runParser parseToContent s f bs
    when (null a) $ throwError "Missing content."
    let xs = map (runCP s . parseVCalendar) a
    (x, w) <- ((flip.).) flip foldM ([], []) xs $ \(x, ws) (g, (pos, _), w) ->
             case g of
                  Left e -> Left $ show pos ++ ": " ++ e
                  Right y -> Right (y:x, w <> ws)
    return (x, w)

-- | Deprecated synonym for parseICalendar
parseICal :: DecodingFunctions
          -> FilePath
          -> ByteString
          -> Either String ([VCalendar], [String])
parseICal = parseICalendar
{-# DEPRECATED parseICal "Use parseICalendar instead" #-}

-- | Parse an iCalendar file.
parseICalendarFile :: DecodingFunctions
                   -> FilePath
                   -> IO (Either String ([VCalendar], [String]))
parseICalendarFile s f = parseICal s f <$> B.readFile f

-- | Deprecated synonym for parseICalendarFile
parseICalFile :: DecodingFunctions
              -> FilePath
              -> IO (Either String ([VCalendar], [String]))
parseICalFile = parseICalendarFile
{-# DEPRECATED parseICalFile "Use parseICalendarFile instead" #-}

runCP :: DecodingFunctions -> ContentParser a
      -> (Either String a, (SourcePos, [Content]), [String])
runCP s = ((flip .) . flip) runRWS s (undefined, undefined) . runExceptT
