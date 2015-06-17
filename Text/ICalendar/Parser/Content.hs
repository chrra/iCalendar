{-# LANGUAGE OverloadedStrings #-}
module Text.ICalendar.Parser.Content
    ( Content(..)
    , DecodingFunctions(..)
    , contentParser
    , parseToContent
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

import qualified Text.Parsec                  as P
import           Text.Parsec.ByteString.Lazy  ()
import           Text.Parsec.Combinator       hiding (optional)
import           Text.Parsec.Prim             hiding (many, (<|>))
import           Text.Parsec.Text.Lazy        ()

import           Text.ICalendar.Parser.Common

parseToContent :: DecodingFunctions
               -> ByteString
               -> Either String [Content]
parseToContent df bs = left show $ runParser contentParser df "-" bs

contentParser :: TextParser [Content]
contentParser = do content <- sepEndBy1 contentline newline
                   f <- dfBS2IText <$> getState
                   return $ componentalize f content

newline :: TextParser ()
newline = (char '\r' >> void (optional $ char '\n')) <|> void (char '\n')


componentalize :: (ByteString -> CI Text) -> [Content] -> [Content]
componentalize f (ContentLine p "BEGIN" [] n:xs) =
    let (com, rest) = break g xs
        g (ContentLine _ "END" [] en) | f en == n' = True
        g _ = False
        n' = f n
     in Component p n' (componentalize f com)
                : componentalize f (drop 1 rest)
componentalize f (x:xs) = x:componentalize f xs
componentalize _ _ = []


-- | Specialized scan function which unfolds lines.
scan :: s -- ^ Initial state.
     -> (s -> Maybe Char -> Maybe (Maybe s))
     -- ^ Nothing: Fail.
     -- Just Nothing: Done, don't use last char.
     -- Just (Just state): Continue, collecting char unless EOF.
     -> TextParser ByteString
scan state f = go state mempty
  where go st buf = do
            _ <- many (try unfold)
            c <- lookAhead (Just <$> P.anyChar <|> Nothing <$ P.eof)
            case (c, f st c) of
                 (_, Nothing) -> mzero
                 (Just c', Just (Just st')) ->
                    P.anyChar *> go st' (buf <> Bu.char8 c')
                 (_, _) -> return $ Bu.toLazyByteString buf

        unfold = (P.char '\r' >> optional (P.char '\n') >> P.oneOf " \t")
             <|> (P.char '\n' >> P.oneOf " \t")

takeWhile1 :: (Char -> Bool) -> TextParser ByteString
takeWhile1 p = scan False f <?> "takeWhile1 ..."
  where f g (Just x) | p x       = Just (Just True)
                     | g         = Just Nothing
                     | otherwise = Nothing
        f g _        | g         = Just Nothing
                     | otherwise = Nothing

char :: Char -> TextParser ByteString
char c = scan True f <?> show c
  where f True x = if Just c == x then Just (Just False) else Nothing
        f False _ = Just Nothing


isControl', isSafe, isValue, isQSafe, isName :: Char -> Bool
isControl' c = c /= '\t' && isControl c
isSafe c = not (isControl' c) && c `notElem` ("\";:,"::String)
isValue c = let n = fromEnum c in n == 32 || n == 9 || (n >= 0x21 && n /= 0x7F)
isQSafe c = isValue c && c /= '"'
isName c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '-'

contentline :: TextParser Content
contentline = do pos <- P.sourceLine <$> getPosition
                 n <- name
                 ps <- many (char ';' >> param)
                 _ <- char ':'
                 val <- value <|> return mempty
                 return $ ContentLine pos n ps val
  where value :: TextParser ByteString
        value = takeWhile1 isValue <?> "value"

        param :: TextParser (CI Text, [Text])
        param = do n <- name
                   _ <- char '='
                   vs <- sepBy1 paramValue (char ',')
                   return (n, vs)

        name :: TextParser (CI Text)
        name = dfBS2IText <$> getState <*> takeWhile1 isName <?> "name"

        paramValue :: TextParser Text
        paramValue = paramtext <|> quotedString

        paramtext :: TextParser Text
        paramtext = dfBS2Text <$> getState <*> takeWhile1 isSafe <?> "paramtext"

        quotedString :: TextParser Text
        quotedString = (do
            _ <- char '"'
            s <- takeWhile1 isQSafe <|> return mempty
            _ <- char '"'
            dfBS2Text <$> getState <*> pure s) <?> "quoted string"
