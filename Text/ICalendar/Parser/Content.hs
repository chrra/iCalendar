{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.ICalendar.Parser.Content
    ( Content(..)
    , DecodingFunctions(..)
    , parseToContent
    -- * Internal
    , contentParser
    ) where

import           Control.Applicative
import           Control.Arrow                (left)
import           Control.Monad
import           Data.ByteString.Lazy         (ByteString)
import qualified Data.ByteString.Lazy.Builder as Bu
import           Data.CaseInsensitive         (CI)
import           Data.Char
import           Data.Monoid
import           Data.Text.Lazy               (Text)

import           Data.Attoparsec.ByteString.Lazy

import           Text.ICalendar.Parser.Common

parseToContent :: DecodingFunctions
               -> ByteString
               -> Either String [Content]
parseToContent df bs = eitherResult $ parse (contentParser df) bs

contentParser :: DecodingFunctions -> Parser [Content]
contentParser = undefined
