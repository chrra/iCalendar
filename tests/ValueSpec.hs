{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ValueSpec where

import           Data.ByteString.Lazy           (ByteString)
import           Data.CaseInsensitive           (CI)
import           Data.Text.Lazy                 (Text)
import           Test.Hspec

import           Text.ICalendar.Content
import           Text.ICalendar.Parameters
import           Text.ICalendar.Values
import           Text.ICalendar.Values.Internal

import           TestCommon


p :: forall a. ICalendarValue a => [(CI Text, [Text])] -> ByteString -> a
p params v = case runValueParser (valueParser :: ValueParser a) 1 params v of
    (Left e, _)   -> error e
    (Right y, []) -> y
    (_, xs)       -> error (show xs)

notP :: forall a. (Show a, ICalendarValue a)
     => [(CI Text, [Text])] -> ByteString -> a -> ()
notP params v _ =
    case runValueParser (valueParser :: ValueParser a) 1 params v of
        (Left _, _)   -> ()
        (Right y, []) -> error $ show y
        (_,       xs) -> error (show xs)

spec :: Spec
spec = do
    describe "BINARY" $ do
        context "parsing" $ do
            it "parses just binary" $
                p [("encoding", ["base64"])] "dGVzdA=="
                    `shouldBe` Binary Nothing "test"
            it "parses binary with a mimetype" $
                p [("encoding", ["base64"]), ("fmttype", ["text/plain"])]
                        "dGVzdA=="
                    `shouldBe` Binary (Just "text/plain") "test"
            it "fails parsing binary with 8BIT encoding" $
                notP [("encoding", ["8BIT"])] "dGVzdA==" (Binary Nothing "test")
                    `shouldBe` ()
            it "fails parsing binary with no encoding" $
                notP [] "dGVzdA==" (Binary Nothing "test")
                    `shouldBe` ()
        context "printing" $
            it "prints binary" $
                valueToByteString (Binary Nothing "test") `shouldBe` "dGVzdA=="
    describe "BOOLEAN" $ do
        context "parsing" $ do
            it "parses true" $
                p [] "TRUE" `shouldBe` True
            it "parses false" $
                p [] "FALSE" `shouldBe` False
            it "fails otherwise" $
                notP [] "flaming death" True `shouldBe` ()
        context "parsing case-insensitively" $ do
            it "parses true" $
                p [] "true" `shouldBe` True
            it "parses false" $
                p [] "fAlse" `shouldBe` False
        context "printing" $ do
            it "prints true" $
                valueToByteString True `shouldBe` "TRUE"
            it "prints false" $
                valueToByteString False `shouldBe` "FALSE"
    describe "CAL-ADDRESS" $ do
        context "parsing" $ do
            it "parses a uri" $
                p [] uri `shouldBe` CalAddress uri'
            it "fails something not a uri" $
                notP [] "david" (CalAddress uri') `shouldBe` ()
        context "printing" $
            it "prints a uri" $
                valueToByteString (CalAddress uri') `shouldBe` uri
    describe "DATE" $ do
        context "parsing" $ do
            it "parses a date" $
                p [] "20160221" `shouldBe` (Date $ read "2016-02-21")
            it "fails garbage" $
                notP [] "Johannesburg" (Date $ read "2016-02-21") `shouldBe` ()
            it "fails an invalid date" $
                notP [] "20160231" (Date $ read "2016-02-21") `shouldBe` ()
        context "printing" $
            it "prints a date" $
                valueToByteString (Date $ read "2016-12-03")
                    `shouldBe` "20161203"
    describe "DATETIME" $ do
        context "parsing" $ do
            it "parses a floating datetime" $
                p [] "20160221T192354" `shouldBe`
                    (FloatingDateTime $ read "2016-02-21 19:23:54")
            it "parses a zoned datetime" $
                p [("TZID", ["Europe/Oslo"])] "20160221T192354"
                    `shouldBe` ZonedDateTime (read "2016-02-21 19:23:54")
                                             (TZID "Europe/Oslo")
            it "parses a UTC datetime" $
                p [] "20160221T192354Z" `shouldBe`
                    (UTCDateTime $ read "2016-02-21 19:23:54")
            it "fails a zoned UTC" $
                notP [("TZID", ["Europe/Oslo"])] "20160221T192354Z"
                        (undefined :: DateTime)
                    `shouldBe` ()
            it "fails garbage" $
                notP [] "Johannesburg" (undefined :: DateTime)
                    `shouldBe` ()
            it "fails an invalid datetime" $
                notP [] "20160231T235959" (undefined :: DateTime)
                    `shouldBe` ()
        context "printing" $ do
            it "prints a floating datetime" $
                valueToByteString (FloatingDateTime $ read "2016-12-03 19:23:54")
                    `shouldBe` "20161203T192354"
            it "prints a UTC datetime" $
                valueToByteString (UTCDateTime $ read "2016-12-03 19:23:54")
                    `shouldBe` "20161203T192354Z"
