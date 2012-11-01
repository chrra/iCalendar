{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.ICalendar.Parser
    ( parseICal
    , parseICalFile
    , DecodingFunctions(..)
    ) where

import Prelude
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.RWS ( runRWS )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Monoid

import Text.Parsec.Prim hiding (many, (<|>))
import Text.Parsec.Pos
import Text.Parsec.ByteString.Lazy ()
import Text.Parsec.Text.Lazy ()

import Text.ICalendar.Types
import Text.ICalendar.Parser.Common
import Text.ICalendar.Parser.Content
import Text.ICalendar.Parser.Components


-- | Parse a ByteString containing iCalendar data.
-- Returns either an error, or a tuple of the result and a list of warnings.
parseICal :: DecodingFunctions
          -> FilePath
          -> ByteString
          -> Either String ([VCalendar], [String])
parseICal s f bs = do
    a <- either (Left . show) Right $ runParser parseToContent s f bs
    when (null a) $ throwError "Missing content."
    let xs = map (runCP s . parseVCalendar) a
    (x, w) <- ((flip.).) flip foldM ([], []) xs $ \(x, ws) (g, (pos, _), w) ->
             case g of
                  Left e -> Left $ show pos ++ ": " ++ e
                  Right y -> Right (y:x, w <> ws)
    return (x, w)

-- | Parse an iCalendar file.
parseICalFile :: DecodingFunctions
              -> FilePath
              -> IO (Either String ([VCalendar], [String]))
parseICalFile s f = parseICal s f <$> B.readFile f

runCP :: DecodingFunctions -> ContentParser a
      -> (Either String a, (SourcePos, [Content]), [String])
runCP s = ((flip .) . flip) runRWS s (undefined, undefined) . runErrorT
