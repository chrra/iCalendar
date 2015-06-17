{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Text.ICalendar.Parser
    ( parseICalendar
    , parseICalendarFile
    , DecodingFunctions(..)
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.RWS          (runRWS)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Monoid
import           Prelude

import Text.Parsec.ByteString.Lazy ()
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
               -> ByteString
               -> Either String ([VCalendar], [String])
parseICalendar s bs = do
    a <- parseToContent s bs
    when (null a) $ throwError "Missing content."
    let xs = map (runCP s . parseVCalendar) a
    (x, w) <- ((flip.).) flip foldM ([], []) xs $ \(x, ws) (g, (pos, _), w) ->
             case g of
                  Left e -> Left $ "Line " ++ show pos ++ ": " ++ e
                  Right y -> Right (y:x, w <> ws)
    return (x, w)

-- | Parse an iCalendar file.
parseICalendarFile :: DecodingFunctions
                   -> FilePath
                   -> IO (Either String ([VCalendar], [String]))
parseICalendarFile s f = parseICalendar s <$> B.readFile f

runCP :: DecodingFunctions -> ContentParser a
      -> (Either String a, (Int, [Content]), [String])
runCP s = ((flip .) . flip) runRWS s (undefined, undefined) . runErrorT
