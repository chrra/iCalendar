{-# LANGUAGE OverloadedStrings #-}
module ContentSpec where

import           Data.ByteString.Lazy          (ByteString)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.CaseInsensitive          as CI
import           Data.Default
import           Data.Monoid                   ((<>))
import           Data.Text.Lazy                (Text)
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.Encoding       as Enc
import           Test.Hspec

import           Text.ICalendar.Parser.Content


p :: ByteString -> [Content]
p = either error id . parseToContent def

notP :: ByteString -> ()
notP = either (const ()) (error . show) . parseToContent def

enc :: Text -> ByteString
enc = Enc.encodeUtf8

foldResult :: [Content]
foldResult = [ContentLine 1 "DESCRIPTION" [] "This is a long description that exists on a long line."]

fold1, fold2, fold3, fold4, fold5, fold6, fold7, fold8 :: ByteString
fold1 = "DESCRIPTION:This is a long description that exists on a long line."

fold2 = "DESCRIPTION:This is a lo\r\n\
\ ng description\r\n\
\  that exists on a long line."

fold3 = "DESCRIPTION:This is a lo\n\
\ ng description\n\
\  that exists on a long line."

fold4 = "DESCRIPTION:This is a lo\r\
\ ng description\r\
\  that exists on a long line."

fold5 = "DESCRIPTION:This is a lo\r\n\
\\tng description\r\n\
\\t that exists on a long line."

fold6 = "DESCRIPTION:This is a lo\r\n\
\ ng description\r\n\
\ \r\n\
\  that exists on a long line."

fold7 = BL.intercalate "\r\n " . map (BL.pack . (:[])) $ BL.unpack fold1

fold8Result :: [Content]
fold8Result = foldResult ++ [ContentLine 4 "A" [] ""]

fold8 = fold2 <> "\r\nA:"

fullFoldResult = [ContentLine 1 "AB" [ ("CD", ["EF", "GH", "IJ"])
                                      ,("KL", ["MN", "OP", "QR"])
                                     ] "STUVWXYZ"]

fullFold1, fullFold2 :: ByteString
fullFold1 = "AB;CD=\"EF\",\"GH\",IJ;KL=MN,OP,\"QR\":STUVWXYZ"

fullFold2 = BL.intercalate "\r\n " . map (BL.pack . (:[])) $ BL.unpack fullFold1

compResult :: [Content]
compResult = [Component 1 "VCALENDAR" [ContentLine 2 "TEST" [] "VALUE"]]

comp1, comp2, comp3 :: ByteString
comp1 = "BEGIN:VCALENDAR\r\n\
\TEST:VALUE\r\n\
\END:VCALENDAR\r\n"

comp2 = "BEGIN:VCALENDAR\r\n\
\TEST:VALUE\r\n\
\END:VCALENDAR"

comp3 = "begin:vcalendar\r\n\
\test:VALUE\r\n\
\EnD:vCaLeNdAr"


spec :: Spec
spec = do
    describe "parseToContent" $ do

        context "line" $ do
            let lineResult = [ContentLine 1 "A" [] ""]
            it "parses a line" $
                p "A:" `shouldBe` lineResult
            it "parses a line case-insensitively" $
                p "a:" `shouldBe` lineResult
            it "fails an invalid line" $
                notP "a" `shouldBe` ()
            it "ignores line terminators" $
                p "A:\r\n" `shouldBe` lineResult

        context "two lines" $ do
            let twoResult = [ContentLine 1 "A" [] "", ContentLine 2 "B" [] ""]
            it "parses two lines" $
                p "A:\r\nB:" `shouldBe` twoResult
            it "accepts LF instead of CRLF" $
                p "A:\nB:" `shouldBe` twoResult
            it "accepts CR instead of CRLF" $
                p "A:\rB:" `shouldBe` twoResult
            it "ignores line terminators" $
                p "A:\r\nB:\r\n" `shouldBe` twoResult
            -- TODO recheck these two
            it "errors on blank lines between lines" $
                notP "A:\r\n\r\nB:" `shouldBe` ()
            it "errors on trailing blank lines" $
                notP "A:\r\nB:\r\n\r\n" `shouldBe` ()

        context "attributes" $ do
            let attrResult = [ContentLine 1 "A" [("X", ["Y"])] "Z"]
            it "parses attributes" $
                p "A;X=Y:Z" `shouldBe` attrResult
            it "parses quoted attributes" $
                p "A;X=\"Y\":Z" `shouldBe` attrResult
            it "fails unclosed quoted attributes" $
                notP "A;X=\"Y:Z" `shouldBe` ()

            it "parses multiple parameter values into a list" $
                p "A;X=Y,Y:Z" `shouldBe` [ContentLine 1 "A" [("X", ["Y","Y"])] "Z"]
            it "parses multiple quoted parameter values into a list" $
                p "A;X=\"Y\",\"Y\":Z" `shouldBe` [ContentLine 1 "A" [("X", ["Y","Y"])] "Z"]
            it "parses multiple mixed quoted and unquoted parameter values into a list" $
                p "A;X=\"Y\",Y:Z" `shouldBe` [ContentLine 1 "A" [("X", ["Y","Y"])] "Z"]

            it "parses UTF8 parameters" $
                p (enc "A;X=Æ:Z") `shouldBe` [ContentLine 1 "A" [("X", ["Æ"])] "Z"]
            it "parses quoted UTF8 parameters" $
                p (enc "A;X=\"Æ\":Z") `shouldBe` [ContentLine 1 "A" [("X", ["Æ"])] "Z"]

            it "fails an illegal semicolon in an unquoted attribute" $
                notP "A;X=C;D:Z" `shouldBe` ()
            it "parses a semicolon in a quoted attribute" $
                p "A;X=\"C;D\":Z" `shouldBe` [ContentLine 1 "A" [("X", ["C;D"])] "Z"]

        context "folding" $ do
            it "parses a single line correctly" $
                p fold1 `shouldBe` foldResult
            it "doesn't chance when a line is folded with CRLF SPACE" $
                p fold2 `shouldBe` foldResult
            it "is lenient when LF instead of CRLF is used" $
                p fold3 `shouldBe` foldResult
            it "is lenient when CR instead of CRLF is used" $
                p fold4 `shouldBe` foldResult
            it "accepts CRLF HTAB folds" $
                p fold5 `shouldBe` foldResult
            it "accepts two folds with nothing between them" $
                p fold6 `shouldBe` foldResult
            it "accepts pathological fold" $
                p fold7 `shouldBe` foldResult
            it "calculates line numbers correctly" $
                p fold8 `shouldBe` fold8Result
            it "accepts a line fold in the middle of a UTF8 sequence" $
                p "A;X=\"\195\r\n\t\152\":Z" `shouldBe`
                    [ContentLine 1 "A" [("X", ["Ø"])] "Z"]


            it "parses a line with all features" $
                p fullFold1 `shouldBe` fullFoldResult
            it "parses a line with all features fully folded" $
                p fullFold2 `shouldBe` fullFoldResult

        context "components" $ do
            it "groups components correctly" $
                p comp1 `shouldBe` compResult
            it "is case insensitive with begin/end and component name" $
                p comp2 `shouldBe` compResult

        context "edge" $ do
            it "parses latin1" $
                parseToContent (DecodingFunctions Enc.decodeLatin1 (CI.mk . Enc.decodeLatin1))
                               "A;X=\"\216\":Z"
                    `shouldBe` (Right [ContentLine 1 "A" [("X", ["Ø"])] "Z"])
            it "fails UTF8 names" $
                notP (enc "Ø:") `shouldBe` ()
            it "fails UTF8 parameter names" $
                notP (enc "A;Ø=A:") `shouldBe` ()
            it "fails UTF8 component names" $
                notP (enc "BEGIN:Æ\r\nEND:Æ") `shouldBe` ()
