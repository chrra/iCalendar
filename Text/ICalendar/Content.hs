{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.ICalendar.Content
    ( Content(..)
    , parseToContent
    -- * Printing iCalendar to content lines.
    , propertyToContent
    , parameterToContent
    , valueToByteString
    , propertyPrinterToContent
    , parameterPrinterToContent
    , valuePrinterToByteString
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Attoparsec.ByteString.Lazy
import           Data.ByteString.Lazy               (ByteString)
import qualified Data.ByteString.Lazy               as LB
import           Data.CaseInsensitive               (CI)
import qualified Data.CaseInsensitive               as CI
import           Data.Text.Lazy                     (Text)
import qualified Data.Text.Lazy.Encoding            as Enc
import           Data.Word                          (Word8)

import           Text.ICalendar.Internal
import           Text.ICalendar.Parameters.Internal
import           Text.ICalendar.Properties.Internal
import           Text.ICalendar.Values.Internal


-- | Convert a property to 'Content'.
propertyToContent :: ICalendarProperty a => a -> [Content]
propertyToContent = propertyPrinterToContent . propertyPrinter

-- | Convert a parameter to what's used in 'Content'.
parameterToContent :: ICalendarParameter a => a -> [(CI Text, [Text])]
parameterToContent = parameterPrinterToContent . parameterPrinter

-- | Convert a value to what's used in 'Content'.
valueToByteString :: ICalendarValue a => a -> ByteString
valueToByteString = valuePrinterToByteString . valuePrinter

-- | Convert a property printer to 'Content'.
propertyPrinterToContent :: PropertyPrinter -> [Content]
propertyPrinterToContent (PropertyPrinter f) = f g
  where
    g :: CI Text -> ParameterPrinter -> ValuePrinter -> [Content]
    g n pp vp = [ ContentLine 1
                              n
                              (parameterPrinterToContent pp)
                              (valuePrinterToByteString vp)
                ]

-- | Convert a parameter printer to what's used in 'Content'.
parameterPrinterToContent :: ParameterPrinter -> [(CI Text, [Text])]
parameterPrinterToContent (ParameterPrinter f) = f g
  where
    g :: CI Text -> [(Quoting, Text)] -> [(CI Text, [Text])]
    g n p = [(n, map snd p)]

-- | Convert a value printer to what's used in 'Content'.
valuePrinterToByteString :: ValuePrinter -> ByteString
valuePrinterToByteString (ValuePrinter g) = g id

type P a = StateT Line Parser a

inc :: P ()
inc = modify' (+1)

-- | Parse a ByteString to content lines.
--
-- Uses UTF-8 encoding.
parseToContent :: ByteString
               -> Either String [Content]
parseToContent bs =
    eitherResult . fmap fst $ parse (runStateT contentParser 1) bs

contentParser :: P [Content]
contentParser = comp =<< linesP <* endP
  where
    linesP = contentLine `sepBy'` (foldP *> newlineP)
    comp = componentalize
    endP = optional (foldP *> newlineP) *> foldP *> lift endOfInput

componentalize :: [Content] -> P [Content]
componentalize (ContentLine p "BEGIN" [] n:xs) =
    if not (LB.all name n)
       then fail $ "invalid component name on line " ++ show p
       else do r <- componentalize com
               when (null rest) .
                   fail $ "Line " ++ show p ++
                            ": mismatched component " ++ show n'
               (Component p n' r:) <$> componentalize (drop 1 rest)
  where
    (com, rest) = break g xs
    g (ContentLine _ "END" [] en) | CI.mk (Enc.decodeUtf8 en) == n' = True
    g _ = False
    n' = CI.mk $ Enc.decodeUtf8 n
componentalize (x:xs) = (x:) <$> componentalize xs
componentalize _      = return []

contentLine :: P Content
contentLine = do
    line <- get
    n <- CI.mk . Enc.decodeUtf8 . LB.pack <$> nameP
    p <- many paramP
    _ <- foldP *> lift (word8 colon)
    v <- LB.pack <$> many (foldP *> lift (satisfy $ \x -> x /= cr && x /= lf))
    return $ ContentLine line n p v
  where
    paramP :: P (CI Text, [Text])
    paramP = foldP *> lift (word8 semicolon)
          *> ((,) <$> (CI.mk . Enc.decodeUtf8 . LB.pack  <$> nameP)
                  <*  foldP <* lift (word8 equals)
                  <*> (paramValueP `sepBy1'` (foldP *> lift (word8 comma))))

    paramValueP :: P Text
    paramValueP = foldP *> (quotedParamValueP <|> unquotedParamValueP)

    quotedParamValueP =
        lift (word8 dquote) *> (Enc.decodeUtf8 . LB.pack
                                    <$> many (foldP *> lift (satisfy qsafe)))
                            <* foldP <* lift (word8 dquote)
    unquotedParamValueP =
        Enc.decodeUtf8 . LB.pack <$> many (foldP *> lift (satisfy safe))

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
