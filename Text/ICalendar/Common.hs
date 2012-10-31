module Text.ICalendar.Common where

import Control.Monad.Error
import Control.Monad.RWS
import Data.ByteString.Lazy (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as TE

import qualified Text.Parsec as P

data Content = ContentLine P.SourcePos (CI Text) [(CI Text, [Text])] ByteString
             | Component P.SourcePos (CI Text) [Content]
               deriving (Show, Eq, Ord)

type TextParser = P.Parsec ByteString DecodingFunctions

type ContentParser = ErrorT String -- Fatal errors.
                            (RWS DecodingFunctions
                                 [String] -- Warnings.
                                 (P.SourcePos, [Content]))

data DecodingFunctions = DecodingFunctions
    { dfBS2Text :: ByteString -> Text
    , dfBS2IText :: ByteString -> CI Text
    }

instance Default DecodingFunctions where
    def = DecodingFunctions TE.decodeUtf8 (CI.mk . TE.decodeUtf8)

