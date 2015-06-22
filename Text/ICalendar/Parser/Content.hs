{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Text.ICalendar.Parser.Content
    ( Content(..)
    , DecodingFunctions(..)
    , parseToContent
    -- * Internal
    , contentParser
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Attoparsec.ByteString.Lazy
import           Data.ByteString.Lazy            (ByteString)
import qualified Data.ByteString.Lazy            as LB
import           Data.CaseInsensitive            (CI)
import           Data.Text.Lazy                  (Text)
import           Data.Word                       (Word8)

import           Text.ICalendar.Parser.Common


type P a = StateT Int Parser a

inc :: P ()
inc = modify' (+1)

parseToContent :: DecodingFunctions
               -> ByteString
               -> Either String [Content]
parseToContent df bs =
    eitherResult . fmap fst $ parse (runStateT (contentParser df) 1) bs

contentParser :: DecodingFunctions -> P [Content]
contentParser df = comp =<< linesP <* endP
  where
    linesP = contentLine df `sepBy'` (foldP *> newlineP)
    comp = componentalize (dfBS2IText df)
    endP = optional (foldP *> newlineP) *> foldP *> lift endOfInput

componentalize :: (ByteString -> CI Text) -> [Content] -> P [Content]
componentalize f (ContentLine p "BEGIN" [] n:xs) =
    if not (LB.all name n)
       then fail $ "invalid component name on line " ++ show p
       else do r <- componentalize f com
               fmap (Component p n' r:) $ componentalize f (drop 1 rest)
  where
    (com, rest) = break g xs
    g (ContentLine _ "END" [] en) | f en == n' = True
    g _ = False
    n' = f n
componentalize f (x:xs) = fmap (x:) $ componentalize f xs
componentalize _ _ = return []

contentLine :: DecodingFunctions -> P Content
contentLine DecodingFunctions {..} = do
    line <- get
    n <- dfBS2IText . LB.pack <$> nameP
    p <- many paramP
    _ <- foldP *> lift (word8 colon)
    v <- LB.pack <$> many (foldP *> lift (satisfy $ \x -> x /= cr && x /= lf))
    return $ ContentLine line n p v
  where
    paramP :: P (CI Text, [Text])
    paramP = foldP *> lift (word8 semicolon)
          *> ((,) <$> (dfBS2IText . LB.pack  <$> nameP)
                  <*  foldP <* lift (word8 equals)
                  <*> (paramValueP `sepBy1'` (foldP *> lift (word8 comma))))

    paramValueP :: P Text
    paramValueP = foldP *> (quotedParamValueP <|> unquotedParamValueP)

    quotedParamValueP =
        lift (word8 dquote) *> (dfBS2Text . LB.pack
                                    <$> many (foldP *> lift (satisfy qsafe)))
                            <* foldP <* lift (word8 dquote)
    unquotedParamValueP =
        dfBS2Text . LB.pack <$> many (foldP *> lift (satisfy safe))

newlineP :: P ()
newlineP = do lift . void $ (optional (word8 cr) *> word8 lf) <|> word8 cr
              inc
{-# INLINE newlineP #-}

foldP :: P ()
foldP = void . many $ newlineP <* lift (satisfy wsp)
{-# INLINE foldP #-}

nameP :: P [Word8]
nameP = many1 $ foldP *> lift (satisfy name <?> "name")

htab, lf, cr, space, dquote, comma, minus, colon, semicolon, equals :: Word8
htab      = 9
lf        = 10
cr        = 13
space     = 32
dquote    = 34
comma     = 44
minus     = 45
colon     = 58
semicolon = 59
equals    = 61

digit, alpha, name, wsp, qsafe, safe :: Word8 -> Bool
digit x = x >= 0x30 && x <= 0x39
alpha x = (x >= 0x41 && x <= 0x5A) || (x >= 0x61 && x <= 0x7A)
name x = digit x || alpha x || x == minus
wsp x = x == space || x == htab

qsafe x = wsp x || (x > 0x20 && x /= dquote && x /= 0x7F)

safe x = wsp x || (x > 0x20 && x /= dquote && x /= semicolon
                            && x /= colon && x /= comma && x /= 0x7F)
