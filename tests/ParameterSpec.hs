{-# LANGUAGE OverloadedStrings #-}
module ParameterSpec where

import           Data.CaseInsensitive               (CI)
import           Data.List                          (sort)
import qualified Data.Set                           as Set
import           Data.String
import           Data.Text.Lazy                     (Text)
import           Test.Hspec

import           Text.ICalendar.Content
import           Text.ICalendar.Parameters
import           Text.ICalendar.Properties.Internal

import           TestCommon

p :: [(CI Text, [Text])] -> PropertyParser a -> a
p i x = case runPropertyParser x 1 i "" of
             (Left e, _)   -> error e
             (Right y, []) -> y
             (_,       xs) -> error (show xs)

pw ::  Show a => [(CI Text, [Text])] -> PropertyParser a -> Int -> a
pw i x n = case runPropertyParser x 1 i "" of
                (Left e, _)                    -> error e
                (Right y, xs) | length xs == n -> y
                              | otherwise      -> error (show y ++ show xs)

notP :: Show a => [(CI Text, [Text])] -> PropertyParser a -> a -> ()
notP i x _ = case runPropertyParser x 1 i "" of
                  (Left _, _)   -> ()
                  (Right y, []) -> error $ show y
                  (_,       xs) -> error (show xs)

spec :: Spec
spec = do
    describe "ALTREP" $ do
        context "parsing" $ do
            it "parses an ALTREP" $
                p [("ALTREP", [uri])] optParam1
                    `shouldBe` Just (AltRep uri')
            it "fails a doubly valued ALTREP" $
                notP [("ALTREP", [uri,uri])]
                     optParam1
                     (Just (AltRep uri'))
                  `shouldBe`
                    ()
            it "is unperturbed by other parameters" $
                p [("TEST", ["a"]), ("ALTREP", [uri])] optParam1
                    `shouldBe` Just (AltRep uri')
            it "fails multiple ALTREPs" $
                notP [("ALTREP", [uri]), ("ALTREP", [uri])]
                     optParam1
                     (Just (AltRep uri'))
                  `shouldBe`
                      ()
            it "is case insensitive" $
                p [("AlTrEp", [uri])] optParam1
                    `shouldBe` Just (AltRep uri')
        context "printing" $
            it "prints an ALTREP" $
                parameterToContent (AltRep uri')
                    `shouldBe` [("ALTREP", [uri])]
    describe "CN" $ do
        context "parsing" $ do
            it "parses a CN" $
                p [("CN", ["Joe"])] optParam1
                    `shouldBe` Just (CN "Joe")
            it "fails a doubly valued CN" $
                notP [("CN", ["Joe", "Mike"])]
                     optParam1
                     (Just (CN ""))
                  `shouldBe`
                    ()
            it "is unperturbed by other parameters" $
                p [("TEST", ["a"]), ("CN", ["Joe"])] optParam1
                    `shouldBe` Just (CN "Joe")
            it "fails multiple CNs" $
                notP [("CN", ["Joe"]), ("CN", ["Mike"])]
                     optParam1
                     (Just (CN ""))
                  `shouldBe`
                      ()
            it "is case insensitive" $
                p [("cN", ["Joe"])] optParam1
                    `shouldBe` Just (CN "Joe")
        context "printing" $
            it "prints a CN" $
                parameterToContent (CN "Joe")
                    `shouldBe` [("CN", ["Joe"])]
    describe "CUTYPE" $ do
        context "parsing" $ do
            it "parses an INDIVIDUAL" $
                p [("CUTYPE", ["INDIVIDUAL"])] optParam1
                    `shouldBe` Just Individual
            it "ignores other parameters" $
                p [("TEST", ["a"]), ("CUTYPE", ["INDIVIDUAL"])] optParam1
                    `shouldBe` Just Individual
            it "parses a GROUP" $
                p [("CUTYPE", ["GROUP"])] optParam1
                    `shouldBe` Just Group
            it "parses a RESOURCE" $
                p [("CUTYPE", ["RESOURCE"])] optParam1
                    `shouldBe` Just Resource
            it "parses a ROOM" $
                p [("CUTYPE", ["ROOM"])] optParam1
                    `shouldBe` Just Room
            it "parses an UNKNOWN" $
                p [("CUTYPE", ["UNKNOWN"])] optParam1
                    `shouldBe` Just Unknown
            it "parses an x-name" $
                p [("CUTYPE", ["X-SPACESTATION"])] optParam1
                    `shouldBe` Just (CUTypeX "X-SPACESTATION")
            it "parses an otherwise unknown iana-name" $
                p [("CUTYPE", ["ROBOT"])] optParam1
                    `shouldBe` Just (CUTypeX "ROBOT")
            it "fails multi-valued CUTYPEs" $
                notP [("CUTYPE", ["ROBOT", "FROM", "THE", "FUTURE"])]
                     optParam1
                     (Just Individual)
                  `shouldBe` ()
        context "parsing case insensitively" $ do
            it "parses an INDIVIDUAL" $
                p [("cUtYpE", ["iNdIvIdUaL"])] optParam1
                    `shouldBe` Just Individual
            it "parses a GROUP" $
                p [("CuTyPe", ["gRoUp"])] optParam1
                    `shouldBe` Just Group
            it "parses a RESOURCE" $
                p [("cUtYpE", ["ResOurCe"])] optParam1
                    `shouldBe` Just Resource
            it "parses a ROOM" $
                p [("cUtYpE", ["rOOm"])] optParam1
                    `shouldBe` Just Room
            it "parses an UNKNOWN" $
                p [("CuTyPe", ["uNkNoWn"])] optParam1
                    `shouldBe` Just Unknown
            it "parses an x-name" $
                p [("CuTYpE", ["x-SpAcEsTaTioN"])] optParam1
                    `shouldBe` Just (CUTypeX "X-sPaCeStAtIOn")
            it "parses an otherwise unknown iana-name" $
                p [("cUtYpE", ["robot"])] optParam1
                    `shouldBe` Just (CUTypeX "ROBOT")
        context "printing" $ do
            it "omits INDIVIDUAL as it's the default" $
                parameterToContent Individual
                    `shouldBe` []
            it "prints a GROUP" $
                parameterToContent Group
                    `shouldBe` [("CUTYPE", ["GROUP"])]
            it "prints a RESOURCE" $
                parameterToContent Resource
                    `shouldBe` [("CUTYPE", ["RESOURCE"])]
            it "prints a ROOM" $
                parameterToContent Room
                    `shouldBe` [("CUTYPE", ["ROOM"])]
            it "prints an UNKNOWN" $
                parameterToContent Unknown
                    `shouldBe` [("CUTYPE", ["UNKNOWN"])]
            it "prints an x-name" $
                parameterToContent (CUTypeX "X-SPACESTATION")
                    `shouldBe` [("CUTYPE", ["X-SPACESTATION"])]
            it "prints an otherwise unknown iana-name" $
                parameterToContent (CUTypeX "ROBOT")
                    `shouldBe` [("CUTYPE", ["ROBOT"])]
    describe "DELEGATED-FROM" $ do
        context "parsing" $ do
            it "parses a single delegated-from" $
                p [("DELEGATED-FROM", ["mailto:joe@example.com"])] optParam1
                    `shouldBe`
                        Just (DelegatedFrom
                                (Set.fromList [u "mailto:joe@example.com"]))
            it "parses multiple delegated-from" $
                p [("DELEGATED-FROM", [ "mailto:joe@example.com"
                                      , "mailto:mike@example.com"])] optParam1
                    `shouldBe`
                        Just (DelegatedFrom
                                (Set.fromList [ u "mailto:joe@example.com"
                                              , u "mailto:mike@example.com"]))
            it "fails invalid URIs" $
                notP [("DELEGATED-FROM", ["mailto_joe@example.com"])]
                     optParam1
                     (Just (DelegatedFrom mempty))
                  `shouldBe`
                        ()
            it "fails invalid URIs even after valid ones" $
                notP [("DELEGATED-FROM", [ "mailto:joe@example.com"
                                         , "mailto_mike@example.com"])]
                      optParam1
                      (Just (DelegatedFrom mempty))
                  `shouldBe`
                        ()
            it "parses a single delegated-from case-insensitively" $
                p [("DeLeGaTeD-FrOm", ["mailto:joe@example.com"])] optParam1
                    `shouldBe`
                        Just (DelegatedFrom
                                (Set.fromList [u "mailto:joe@example.com"]))
        context "printing" $ do
            it "prints a single delegated-from" $
                parameterToContent
                    (DelegatedFrom (Set.fromList [u "mailto:joe@example.com"]))
                  `shouldBe`
                    [("DELEGATED-FROM", ["mailto:joe@example.com"])]
            it "prints multiple delegated-from" $
                parameterToContent
                    (DelegatedFrom
                        (Set.fromList [ u "mailto:joe@example.com"
                                      , u "mailto:mike@example.com"]))
                  `shouldBe`
                    [("DELEGATED-FROM", [ "mailto:joe@example.com"
                                        , "mailto:mike@example.com"])]
            it "omits empty delegated-from" $
                parameterToContent (DelegatedFrom mempty)
                    `shouldBe` []
    describe "DELEGATED-TO" $ do
        context "parsing" $ do
            it "parses a single delegated-from" $
                p [("DELEGATED-TO", ["mailto:joe@example.com"])] optParam1
                    `shouldBe`
                        Just (DelegatedTo
                                (Set.fromList [u "mailto:joe@example.com"]))
            it "parses multiple delegated-to" $
                p [("DELEGATED-TO", [ "mailto:joe@example.com"
                                      , "mailto:mike@example.com"])] optParam1
                    `shouldBe`
                        Just (DelegatedTo
                                (Set.fromList [ u "mailto:joe@example.com"
                                              , u "mailto:mike@example.com"]))
            it "fails invalid URIs" $
                notP [("DELEGATED-TO", ["mailto_joe@example.com"])]
                     optParam1
                     (Just (DelegatedTo mempty))
                  `shouldBe`
                        ()
            it "fails invalid URIs even after valid ones" $
                notP [("DELEGATED-TO", [ "mailto:joe@example.com"
                                         , "mailto_mike@example.com"])]
                      optParam1
                      (Just (DelegatedTo mempty))
                  `shouldBe`
                        ()
            it "parses a single delegated-to case-insensitively" $
                p [("DeLeGaTeD-To", ["mailto:joe@example.com"])] optParam1
                    `shouldBe`
                        Just (DelegatedTo
                                (Set.fromList [u "mailto:joe@example.com"]))
        context "printing" $ do
            it "prints a single delegated-to" $
                parameterToContent
                    (DelegatedTo (Set.fromList [u "mailto:joe@example.com"]))
                  `shouldBe`
                    [("DELEGATED-TO", ["mailto:joe@example.com"])]
            it "prints multiple delegated-to" $
                parameterToContent
                    (DelegatedTo
                        (Set.fromList [ u "mailto:joe@example.com"
                                      , u "mailto:mike@example.com"]))
                  `shouldBe`
                    [("DELEGATED-TO", [ "mailto:joe@example.com"
                                      , "mailto:mike@example.com"])]
            it "omits empty delegated-To" $
                parameterToContent (DelegatedTo mempty)
                    `shouldBe` []
    let ldap :: IsString a => a
        ldap = "ldap://example.com:6666/o=ABC%20Industries,c=US???(cn=Jim%20Dolittle)"
    describe "DIR" $ do
        context "parsing" $ do
            it "parses a DIR" $
                p [("DIR", [ldap])]
                  optParam1
                    `shouldBe` Just (Dir (u ldap))
            it "fails a doubly valued DIR" $
                notP [("DIR", [uri, uri])]
                     optParam1
                     (Just (Dir undefined))
                  `shouldBe`
                    ()
            it "fails a DIR with an invalid URI" $
                notP [("DIR", ["wat"])]
                     optParam1
                     (Just (Dir undefined))
                  `shouldBe`
                    ()
            it "is unperturbed by other parameters" $
                p [("TEST", ["a"]), ("DIR", [uri])] optParam1
                    `shouldBe` Just (Dir uri')
            it "fails multiple DIRs" $
                notP [("DIR", [uri]), ("DIR", [uri])]
                     optParam1
                     (Just (Dir undefined))
                  `shouldBe`
                      ()
            it "is case insensitive" $
                p [("DiR", [uri])] optParam1
                    `shouldBe` Just (Dir uri')
        context "printing" $
            it "prints a DIR" $
                parameterToContent (Dir (u ldap))
                    `shouldBe` [("DIR", [ldap])]
    describe "ENCODING" $ do
        context "parsing" $ do
            it "parses 8BIT" $
                p [("ENCODING", ["8BIT"])] optParam1
                    `shouldBe` Just EightBit
            it "parses BASE64" $
                p [("ENCODING", ["BASE64"])] optParam1
                    `shouldBe` Just Base64
            it "parses default 8BIT" $
                p [] defParam1 `shouldBe` EightBit
        context "parsing case-insensitively" $ do
            it "parses 8BIT" $
                p [("encoding", ["8bit"])] optParam1
                    `shouldBe` Just EightBit
            it "parses BASE64" $
                p [("EnCoDiNg", ["base64"])] optParam1
                    `shouldBe` Just Base64
        context "printing" $ do
            it "prints no 8bit, as it's the default" $
                parameterToContent EightBit `shouldBe` []
            it "prints base64" $
                parameterToContent Base64 `shouldBe` [("ENCODING",["BASE64"])]
    describe "FMTTYPE" $ do
        context "parsing" $
            it "parses a mimetype" $
                p [("FMTTYPE", [mimeType])] optParam1
                    `shouldBe` Just (mimeType :: FMTType)
        context "printing" $
            it "prints a mimetype" $
                parameterToContent (mimeType :: FMTType)
                    `shouldBe` [("FMTTYPE", [mimeType])]
    describe "FBTYPE" $ do
        context "parsing" $ do
            it "parses FREE" $
                p [("FBTYPE", ["FREE"])] defParam1
                    `shouldBe` Free
            it "parses BUSY" $
                p [("FBTYPE", ["BUSY"])] defParam1
                    `shouldBe` Busy
            it "BUSY is the default" $
                p [] defParam1 `shouldBe` Busy
            it "parses BUSY-UNAVAILABLE" $
                p [("FBTYPE", ["BUSY-UNAVAILABLE"])] defParam1
                    `shouldBe` BusyUnavailable
            it "parses BUSY-TENTATIVE" $
                p [("FBTYPE", ["BUSY-TENTATIVE"])] defParam1
                    `shouldBe` BusyTentative
            it "parses x-name" $
                p [("FBTYPE", ["X-OTHERWISE-INDISPOSED"])] defParam1
                    `shouldBe` FBTypeX "X-OTHERWISE-INDISPOSED"
            it "parses other unknown iana-token" $
                p [("FBTYPE", ["DEAD"])] defParam1
                    `shouldBe` FBTypeX "DEAD"
            it "fails multiple values" $
                notP [("FBTYPE", ["FREE", "FREE"])] defParam1 Free
                    `shouldBe` ()
            it "fails multiple FBTYPEs" $
                notP [("FBTYPE", ["FREE"]), ("FBTYPE", ["FREE"])]
                     defParam1  Free
                  `shouldBe` ()
        context "parsing case-insensitively" $ do
            it "parses FREE" $
                p [("fBtYpE", ["fReE"])] defParam1
                    `shouldBe` Free
            it "parses BUSY" $
                p [("fBtYpE", ["BuSy"])] defParam1
                    `shouldBe` Busy
            it "parses BUSY-UNAVAILABLE" $
                p [("FbTyPe", ["bUsY-UnAvAiLaBlE"])] defParam1
                    `shouldBe` BusyUnavailable
            it "parses BUSY-TENTATIVE" $
                p [("FbTyPe", ["BuSy-TeNtAtIvE"])] defParam1
                    `shouldBe` BusyTentative
            it "parses x-name" $
                p [("fBtYpE", ["x-OtHeRwIsE-iNdIsPoSeD"])] defParam1
                    `shouldBe` FBTypeX "X-OTHERWISE-INDISPOSED"
            it "parses other unknown iana-token" $
                p [("fbtype", ["dead"])] defParam1
                    `shouldBe` FBTypeX "DEAD"
        context "printing" $ do
            it "prints FREE" $
                parameterToContent Free
                    `shouldBe` [("FBTYPE", ["FREE"])]
            it "omits BUSY as it's the default" $
                parameterToContent Busy
                    `shouldBe` []
            it "prints BUSY-UNAVAILABLE" $
                parameterToContent BusyUnavailable
                    `shouldBe` [("FBTYPE", ["BUSY-UNAVAILABLE"])]
            it "prints BUSY-TENTATIVE" $
                parameterToContent BusyTentative
                    `shouldBe` [("FBTYPE", ["BUSY-TENTATIVE"])]
            it "prints an x-name" $
                parameterToContent (FBTypeX "X-OTHERWISE-INDISPOSED")
                    `shouldBe` [("FBTYPE", ["X-OTHERWISE-INDISPOSED"])]
            it "prints an other unknown iana-token" $
                parameterToContent (FBTypeX "DEAD")
                    `shouldBe` [("FBTYPE", ["DEAD"])]
    describe "LANGUAGE" $ do
        context "parsing" $ do
            it "parses a LANGUAGE" $
                p [("LANGUAGE", ["no"])] optParam1
                    `shouldBe` Just (Language "no")
            it "fails a doubly valued LANGUAGE" $
                notP [("LANGUAGE", ["no", "nb"])]
                     optParam1
                     (Just (Language ""))
                  `shouldBe`
                    ()
            it "is unperturbed by other parameters" $
                p [("TEST", ["a"]), ("LANGUAGE", ["no"])] optParam1
                    `shouldBe` Just (Language "no")
            it "fails multiple LANGUAGEs" $
                notP [("LANGUAGE", ["no"]), ("LANGUAGE", ["nb"])]
                     optParam1
                     (Just (Language ""))
                  `shouldBe`
                      ()
            it "is case insensitive" $
                p [("LaNgUaGe", ["en-gb"])] optParam1
                    `shouldBe` Just (Language "en-GB")
        context "printing" $
            it "prints a LANGUAGE" $
                parameterToContent (Language "en-GB")
                    `shouldBe` [("LANGUAGE", ["en-GB"])]
    describe "MEMBER" $ do
        context "parsing" $ do
            it "parses a single member" $
                p [("MEMBER", ["mailto:ietf-calsch@example.org"])] optParam1
                    `shouldBe`
                        Just (Member
                                (Set.fromList [u "mailto:ietf-calsch@example.org"]))
            it "parses multiple member" $
                p [("MEMBER", [ "mailto:projectA@example.com"
                              , "mailto:projectB@example.com"])] optParam1
                    `shouldBe`
                        Just (Member
                                (Set.fromList [ u "mailto:projectA@example.com"
                                              , u "mailto:projectB@example.com"]))
            it "fails invalid URIs" $
                notP [("DELEGATED-TO", ["ietf-calsch@example.org"])]
                     optParam1
                     (Just (DelegatedTo mempty))
                  `shouldBe`
                        ()
            it "fails invalid URIs even after valid ones" $
                notP [("DELEGATED-TO", [ "mailto:projectA@example.com"
                                         , "projectB@example.com"])]
                      optParam1
                      (Just (DelegatedTo mempty))
                  `shouldBe`
                        ()
            it "parses a single MEMBER case-insensitively" $
                p [("MeMbEr", ["mailto:projectA@example.com"])] optParam1
                    `shouldBe`
                        Just (Member
                                (Set.fromList [u "mailto:projectA@example.com"]))
        context "printing" $ do
            it "prints a single member" $
                parameterToContent
                    (Member (Set.fromList [u "mailto:ietf-calsch@example.org"]))
                  `shouldBe`
                    [("MEMBER", ["mailto:ietf-calsch@example.org"])]
            it "prints multiple member" $
                parameterToContent
                    (Member
                        (Set.fromList [ u "mailto:projectA@example.com"
                                      , u "mailto:projectB@example.com"]))
                  `shouldBe`
                    [("MEMBER", [ "mailto:projectA@example.com"
                                , "mailto:projectB@example.com"])]
            it "omits empty MEMBER" $
                parameterToContent (Member mempty)
                    `shouldBe` []
    describe "PARTSTAT for VEVENTs" $ do
        context "parsing" $ do
            it "parses NEEDS-ACTION" $
                p [("PARTSTAT", ["NEEDS-ACTION"])] defParam1
                    `shouldBe` EventNeedsAction
            it "gives NEEDS-ACTION for empty as it's the default" $
                p [] defParam1
                    `shouldBe` EventNeedsAction
            it "parses ACCEPTED" $
                p [("PARTSTAT", ["ACCEPTED"])] defParam1
                    `shouldBe` EventAccepted
            it "parses DECLINED" $
                p [("PARTSTAT", ["DECLINED"])] defParam1
                    `shouldBe` EventDeclined
            it "parses TENTATIVE" $
                p [("PARTSTAT", ["TENTATIVE"])] defParam1
                    `shouldBe` EventTentative
            it "parses DELEGATED" $
                p [("PARTSTAT", ["DELEGATED"])] defParam1
                    `shouldBe` EventDelegated
            it "parses COMPLETED as other iana-token" $
                p [("PARTSTAT", ["COMPLETED"])] defParam1
                    `shouldBe` EventPartStatX "COMPLETED"
            it "parses IN-PROCESS as other iana-token" $
                p [("PARTSTAT", ["IN-PROCESS"])] defParam1
                    `shouldBe` EventPartStatX "IN-PROCESS"
            it "parses x-name" $
                p [("PARTSTAT", ["X-DETAINED-AT-AIRPORT"])] defParam1
                    `shouldBe` EventPartStatX "X-DETAINED-AT-AIRPORT"
            it "parses unknown iana-token" $
                p [("PARTSTAT", ["DEAD"])] defParam1
                    `shouldBe` EventPartStatX "DEAD"
            it "fails multiple values" $
                notP [("PARTSTAT", ["ACCEPTED", "DECLINED"])]
                     defParam1
                     EventDeclined
                  `shouldBe` ()
            it "fails multiple PARTSTATs" $
                notP [("PARTSTAT", ["ACCEPTED"]), ("PARTSTAT", ["ACCEPTED"])]
                     defParam1  EventAccepted
                  `shouldBe` ()
        context "parsing case insensitively" $ do
            it "parses NEEDS-ACTION" $
                p [("pArTsTaT", ["NeEdS-acTion"])] defParam1
                    `shouldBe` EventNeedsAction
            it "parses ACCEPTED" $
                p [("pArTsTAt", ["acCEpTeD"])] defParam1
                    `shouldBe` EventAccepted
            it "parses DECLINED" $
                p [("pArTStAt", ["dEClIned"])] defParam1
                    `shouldBe` EventDeclined
            it "parses TENTATIVE" $
                p [("PaRTstAt", ["tEnTAtivE"])] defParam1
                    `shouldBe` EventTentative
            it "parses DELEGATED" $
                p [("pArTstAt", ["dElEgAtEd"])] defParam1
                    `shouldBe` EventDelegated
        context "printing" $ do
            it "omits NEEDS-ACTION as it's the default" $
                parameterToContent EventNeedsAction
                    `shouldBe` []
            it "prints ACCEPTED" $
                parameterToContent EventAccepted
                    `shouldBe` [("PARTSTAT", ["ACCEPTED"])]
            it "prints DECLINED" $
                parameterToContent EventDeclined
                    `shouldBe` [("PARTSTAT", ["DECLINED"])]
            it "prints TENTATIVE" $
                parameterToContent EventTentative
                    `shouldBe` [("PARTSTAT", ["TENTATIVE"])]
            it "prints DELEGATED" $
                parameterToContent EventDelegated
                    `shouldBe` [("PARTSTAT", ["DELEGATED"])]
            it "prints x-name" $
                parameterToContent (TodoPartStatX "X-DETAINED-AT-AIRPORT")
                    `shouldBe` [("PARTSTAT", ["X-DETAINED-AT-AIRPORT"])]
            it "prints unknown iana-token" $
                parameterToContent (TodoPartStatX "DEAD")
                    `shouldBe` [("PARTSTAT", ["DEAD"])]
    describe "PARTSTAT for VTODOs" $ do
        context "parsing" $ do
            it "parses NEEDS-ACTION" $
                p [("PARTSTAT", ["NEEDS-ACTION"])] defParam1
                    `shouldBe` PartStatTodoNeedsAction
            it "gives NEEDS-ACTION for empty as it's the default" $
                p [] defParam1
                    `shouldBe` PartStatTodoNeedsAction
            it "parses ACCEPTED" $
                p [("PARTSTAT", ["ACCEPTED"])] defParam1
                    `shouldBe` TodoAccepted
            it "parses DECLINED" $
                p [("PARTSTAT", ["DECLINED"])] defParam1
                    `shouldBe` TodoDeclined
            it "parses TENTATIVE" $
                p [("PARTSTAT", ["TENTATIVE"])] defParam1
                    `shouldBe` TodoTentative
            it "parses DELEGATED" $
                p [("PARTSTAT", ["DELEGATED"])] defParam1
                    `shouldBe` TodoDelegated
            it "parses COMPLETED" $
                p [("PARTSTAT", ["COMPLETED"])] defParam1
                    `shouldBe` TodoCompleted
            it "parses IN-PROCESS" $
                p [("PARTSTAT", ["IN-PROCESS"])] defParam1
                    `shouldBe` TodoInProcess
            it "parses x-name" $
                p [("PARTSTAT", ["X-SEPPUKUD"])] defParam1
                    `shouldBe` TodoPartStatX "X-SEPPUKUD"
            it "parses unknown iana-token" $
                p [("PARTSTAT", ["DISHONORABLY-GIVEN-UP"])] defParam1
                    `shouldBe` TodoPartStatX "DISHONORABLY-GIVEN-UP"
            -- TODO
            -- it "warns illegal characters in x-names" $
            --     pw [("PARTSTAT", ["x-a_"])] defParam1 1
            --         `shouldBe` TodoPartStatX "x-a_"
            it "fails multiple values" $
                notP [("PARTSTAT", ["ACCEPTED", "DECLINED"])]
                     defParam1
                     TodoDeclined
                  `shouldBe` ()
            it "fails multiple PARTSTATs" $
                notP [("PARTSTAT", ["ACCEPTED"]), ("PARTSTAT", ["ACCEPTED"])]
                     defParam1  TodoAccepted
                  `shouldBe` ()
        context "parsing case insensitively" $ do
            it "parses NEEDS-ACTION" $
                p [("PaRtStAt", ["NeEdS-Action"])] defParam1
                    `shouldBe` PartStatTodoNeedsAction
            it "parses ACCEPTED" $
                p [("pArTsTaT", ["aCCePTeD"])] defParam1
                    `shouldBe` TodoAccepted
            it "parses DECLINED" $
                p [("PaRtStaT", ["deClinEd"])] defParam1
                    `shouldBe` TodoDeclined
            it "parses TENTATIVE" $
                p [("ParTStaT", ["TenTativE"])] defParam1
                    `shouldBe` TodoTentative
            it "parses DELEGATED" $
                p [("pARtSTaT", ["DElEGAtEd"])] defParam1
                    `shouldBe` TodoDelegated
            it "parses COMPLETED" $
                p [("pArTsTat", ["completeD"])] defParam1
                    `shouldBe` TodoCompleted
            it "parses IN-PROCESS" $
                p [("parTsTaT", ["iN-ProCess"])] defParam1
                    `shouldBe` TodoInProcess
            it "parses x-name" $
                p [("pARtStaT", ["x-SEpPuKu'D"])] defParam1
                    `shouldBe` TodoPartStatX "X-SEPPUKU'D"
            it "parses unknown iana-token" $
                p [("pARtSTat", ["Dishonorably-Given-Up"])] defParam1
                    `shouldBe` TodoPartStatX "DISHONORABLY-GIVEN-UP"
        context "printing" $ do
            it "omits NEEDS-ACTION as it's the default" $
                parameterToContent PartStatTodoNeedsAction
                    `shouldBe` []
            it "prints ACCEPTED" $
                parameterToContent TodoAccepted
                    `shouldBe` [("PARTSTAT", ["ACCEPTED"])]
            it "prints DECLINED" $
                parameterToContent TodoDeclined
                    `shouldBe` [("PARTSTAT", ["DECLINED"])]
            it "prints TENTATIVE" $
                parameterToContent TodoTentative
                    `shouldBe` [("PARTSTAT", ["TENTATIVE"])]
            it "prints DELEGATED" $
                parameterToContent TodoDelegated
                    `shouldBe` [("PARTSTAT", ["DELEGATED"])]
            it "prints COMPLETED" $
                parameterToContent TodoCompleted
                    `shouldBe` [("PARTSTAT", ["COMPLETED"])]
            it "prints IN-PROCESS" $
                parameterToContent TodoInProcess
                    `shouldBe` [("PARTSTAT", ["IN-PROCESS"])]
            it "prints x-name" $
                parameterToContent (TodoPartStatX "X-SEPPUKU'D")
                    `shouldBe` [("PARTSTAT", ["X-SEPPUKU'D"])]
            it "prints unknown iana-token" $
                parameterToContent (TodoPartStatX "DISHONORABLY-GIVEN-UP")
                    `shouldBe` [("PARTSTAT", ["DISHONORABLY-GIVEN-UP"])]
    describe "PARTSTAT for VJOURNAL" $ do
        context "parsing" $ do
            it "parses NEEDS-ACTION" $
                p [("PARTSTAT", ["NEEDS-ACTION"])] defParam1
                    `shouldBe` JournalNeedsAction
            it "gives NEEDS-ACTION for empty as it's the default" $
                p [] defParam1
                    `shouldBe` JournalNeedsAction
            it "parses ACCEPTED" $
                p [("PARTSTAT", ["ACCEPTED"])] defParam1
                    `shouldBe` JournalAccepted
            it "parses DECLINED" $
                p [("PARTSTAT", ["DECLINED"])] defParam1
                    `shouldBe` JournalDeclined
            it "parses TENTATIVE as other iana-token" $
                p [("PARTSTAT", ["TENTATIVE"])] defParam1
                    `shouldBe` JournalPartStatX "TENTATIVE"
            it "parses DELEGATED as other iana-token" $
                p [("PARTSTAT", ["DELEGATED"])] defParam1
                    `shouldBe` JournalPartStatX "DELEGATED"
            it "parses COMPLETED as other iana-token" $
                p [("PARTSTAT", ["COMPLETED"])] defParam1
                    `shouldBe` JournalPartStatX "COMPLETED"
            it "parses IN-PROCESS as other iana-token" $
                p [("PARTSTAT", ["IN-PROCESS"])] defParam1
                    `shouldBe` JournalPartStatX "IN-PROCESS"
            it "parses x-name" $
                p [("PARTSTAT", ["X-THERE-IN-SPIRIT"])] defParam1
                    `shouldBe` JournalPartStatX "X-THERE-IN-SPIRIT"
            it "parses unknown iana-token" $
                p [("PARTSTAT", ["UNKNOWN"])] defParam1
                    `shouldBe` JournalPartStatX "UNKNOWN"
            it "fails multiple values" $
                notP [("PARTSTAT", ["ACCEPTED", "DECLINED"])]
                     defParam1
                     JournalDeclined
                  `shouldBe` ()
            it "fails multiple PARTSTATs" $
                notP [("PARTSTAT", ["ACCEPTED"]), ("PARTSTAT", ["ACCEPTED"])]
                     defParam1  JournalAccepted
                  `shouldBe` ()
        context "parsing case insensitively" $ do
            it "parses NEEDS-ACTION" $
                p [("pArTsTaT", ["NeEdS-acTion"])] defParam1
                    `shouldBe` JournalNeedsAction
            it "parses ACCEPTED" $
                p [("pArTsTAt", ["acCEpTeD"])] defParam1
                    `shouldBe` JournalAccepted
            it "parses DECLINED" $
                p [("pArTStAt", ["dEClIned"])] defParam1
                    `shouldBe` JournalDeclined
        context "printing" $ do
            it "omits NEEDS-ACTION as it's the default" $
                parameterToContent JournalNeedsAction
                    `shouldBe` []
            it "prints ACCEPTED" $
                parameterToContent JournalAccepted
                    `shouldBe` [("PARTSTAT", ["ACCEPTED"])]
            it "prints DECLINED" $
                parameterToContent JournalDeclined
                    `shouldBe` [("PARTSTAT", ["DECLINED"])]
            it "prints x-name" $
                parameterToContent (TodoPartStatX "X-THERE-IN-SPIRIT")
                    `shouldBe` [("PARTSTAT", ["X-THERE-IN-SPIRIT"])]
            it "prints unknown iana-token" $
                parameterToContent (TodoPartStatX "UNKNOWN")
                    `shouldBe` [("PARTSTAT", ["UNKNOWN"])]
    describe "RANGE" $ do
        context "parsing" $ do
            it "parses THISANDFUTURE" $
                p [("RANGE", ["THISANDFUTURE"])] optParam1
                    `shouldBe` Just ThisAndFuture
            it "parses and warns for THISANDPRIOR" $
                pw [("RANGE", ["THISANDPRIOR"])] optParam1 1
                    `shouldBe` Just ThisAndPrior
            it "fails something else" $
                notP [("RANGE", ["JUSTTHIS"])] optParam1 (Just ThisAndFuture)
                    `shouldBe` ()
            it "fails multiple values" $
                notP [("RANGE", ["JUSTTHIS"])] optParam1 (Just ThisAndFuture)
                    `shouldBe` ()
            it "gives Nothing when not present" $
                p [("TEST", ["X"])] optParam1
                    `shouldBe` (Nothing :: Maybe Range)
        context "parsing case insensitively" $ do
            it "parses THISANDFUTURE" $
                p [("RaNgE", ["tHiSaNdFuTure"])] optParam1
                    `shouldBe` Just ThisAndFuture
            it "parses and warns for THISANDPRIOR" $
                pw [("rAnGe", ["tHIsAndpRIor"])] optParam1 1
                    `shouldBe` Just ThisAndPrior
        context "printing" $ do
            it "prints THISANDFUTURE" $
                parameterToContent ThisAndFuture
                    `shouldBe` [("RANGE", ["THISANDFUTURE"])]
            it "does not produce THISANDPRIOR" $
                parameterToContent ThisAndPrior
                    `shouldBe` []
    describe "RELATED" $ do
        context "parsing" $ do
            it "parses START" $
                p [("RELATED", ["START"])] optParam1
                    `shouldBe` Just Start
            it "parses END" $
                p [("RELATED", ["END"])] optParam1
                    `shouldBe` Just End
            it "fails something else" $
                notP [("RELATED", ["JUSTTHIS"])] optParam1 (Just Start)
                    `shouldBe` ()
            it "gives Nothing when not present" $
                p [("TEST", ["X"])] optParam1
                    `shouldBe` (Nothing :: Maybe Related)
        context "printing" $ do
            it "doesn't print START as it's the default" $
                parameterToContent Start
                    `shouldBe` []
            it "prints END" $
                parameterToContent End
                    `shouldBe` [("RELATED", ["END"])]
    describe "RELTYPE" $ do
        context "parsing" $ do
            it "parses PARENT" $
                p [("RELTYPE", ["PARENT"])] optParam1
                    `shouldBe` Just Parent
            it "parses CHILD" $
                p [("RELTYPE", ["CHILD"])] optParam1
                    `shouldBe` Just Child
            it "parses SIBLING" $
                p [("RELTYPE", ["SIBLING"])] optParam1
                    `shouldBe` Just Sibling
            it "parses unrecognized iana-token" $
                p [("RELTYPE", ["COUSIN"])] optParam1
                    `shouldBe` Just (RelTypeX "COUSIN")
            it "parses unrecognized x-name" $
                p [("RELTYPE", ["X-GRANDFATHERPARADOXICALLY"])] optParam1
                    `shouldBe` Just (RelTypeX "X-GRANDFATHERPARADOXICALLY")
        context "parsing case insentitively" $ do
            it "parses PARENT" $
                p [("RELTYPE", ["pArENt"])] optParam1
                    `shouldBe` Just Parent
            it "parses CHILD" $
                p [("RELTYPE", ["ChIlD"])] optParam1
                    `shouldBe` Just Child
            it "parses SIBLING" $
                p [("RELTYPE", ["sIbLing"])] optParam1
                    `shouldBe` Just Sibling
            it "parses unrecognized iana-token" $
                p [("RELTYPE", ["cOusin"])] optParam1
                    `shouldBe` Just (RelTypeX "COUSIN")
            it "parses unrecognized x-name" $
                p [("RELTYPE", ["x-GrandfathErpARaDoXICaLly"])] optParam1
                    `shouldBe` Just (RelTypeX "X-GRANDFATHERPARADOXICALLY")
        context "printing" $ do
            it "doesn't print PARENT as it's the default" $
                parameterToContent Parent
                    `shouldBe` []
            it "prints CHILD" $
                parameterToContent Child
                    `shouldBe` [("RELTYPE", ["CHILD"])]
            it "prints SIBLING" $
                parameterToContent Sibling
                    `shouldBe` [("RELTYPE", ["SIBLING"])]
            it "prints unrecognized iana-token" $
                parameterToContent (RelTypeX "COUSIN")
                    `shouldBe` [("RELTYPE", ["COUSIN"])]
            it "prints unrecognized x-name" $
                parameterToContent (RelTypeX "X-GRANDFATHERPARADOXICALLY")
                    `shouldBe` [("RELTYPE", ["X-GRANDFATHERPARADOXICALLY"])]
    describe "ROLE" $ do
        context "parsing" $ do
            it "parses CHAIR" $
                p [("ROLE", ["CHAIR"])] optParam1
                    `shouldBe` Just Chair
            it "parses REQ-PARTICIPANT" $
                p [("ROLE", ["REQ-PARTICIPANT"])] optParam1
                    `shouldBe` Just ReqParticipant
            it "parses OPT-PARTICIPANT" $
                p [("ROLE", ["OPT-PARTICIPANT"])] optParam1
                    `shouldBe` Just OptParticipant
            it "parses NON-PARTICIPANT" $
                p [("ROLE", ["NON-PARTICIPANT"])] optParam1
                    `shouldBe` Just NonParticipant
            it "parses unrecognized iana-token" $
                p [("ROLE", ["REMOTE-PARTICIPANT"])] optParam1
                    `shouldBe` Just (RoleX "REMOTE-PARTICIPANT")
            it "parses unrecognized x-name" $
                p [("ROLE", ["X-DEAD-PARTICIPANT"])] optParam1 -- funeral
                    `shouldBe` Just (RoleX "X-DEAD-PARTICIPANT")
        context "parsing case insentitively" $ do
            it "parses CHAIR" $
                p [("rOlE", ["cHaIr"])] optParam1
                    `shouldBe` Just Chair
            it "parses REQ-PARTICIPANT" $
                p [("role", ["req-participant"])] optParam1
                    `shouldBe` Just ReqParticipant
            it "parses OPT-PARTICIPANT" $
                p [("RoLe", ["opt-PARTICIPANT"])] optParam1
                    `shouldBe` Just OptParticipant
            it "parses NON-PARTICIPANT" $
                p [("rOLe", ["NoN-PaRticIpaNt"])] optParam1
                    `shouldBe` Just NonParticipant
            it "parses unrecognized iana-token" $
                p [("rOlE", ["rEmOtE-pARTiCIpAnT"])] optParam1
                    `shouldBe` Just (RoleX "REMOTE-PARTICIPANT")
            it "parses unrecognized x-name" $
                p [("rOLe", ["x-DEAD-participant"])] optParam1
                    `shouldBe` Just (RoleX "X-DEAD-PARTICIPANT")
        context "printing" $ do
            it "prints CHAIR" $
                parameterToContent Chair
                    `shouldBe` [("ROLE", ["CHAIR"])]
            it "doesn't print REQ-PARTICIPANT as it's the default" $
                parameterToContent ReqParticipant
                    `shouldBe` []
            it "prints OPT-PARTICIPANT" $
                parameterToContent OptParticipant
                    `shouldBe` [("ROLE", ["OPT-PARTICIPANT"])]
            it "prints NON-PARTICIPANT" $
                parameterToContent NonParticipant
                    `shouldBe` [("ROLE", ["NON-PARTICIPANT"])]
            it "prints unrecognized iana-token" $
                parameterToContent (RoleX "REMOTE-PARTICIPANT")
                    `shouldBe` [("ROLE", ["REMOTE-PARTICIPANT"])]
            it "prints unrecognized x-name" $
                parameterToContent (RoleX "X-DEAD-PARTICIPANT")
                    `shouldBe` [("ROLE", ["X-DEAD-PARTICIPANT"])]
    describe "RSVP" $ do
        context "parsing" $ do
            it "parses TRUE" $
                p [("RSVP", ["TRUE"])] reqParam1
                    `shouldBe` RSVP True
            it "parses FALSE" $
                p [("RSVP", ["FALSE"])] reqParam1
                    `shouldBe` RSVP False
            it "fails others" $
                notP [("RSVP", ["MAYBE"])] reqParam1 (RSVP False)
                    `shouldBe` ()
        context "parsing case insentitively" $ do
            it "parses TRUE" $
                p [("rsvp", ["true"])] reqParam1
                    `shouldBe` RSVP True
            it "parses FALSE" $
                p [("RsVp", ["fAlSe"])] reqParam1
                    `shouldBe` RSVP False
        context "printing" $ do
            it "prints TRUE" $
                parameterToContent (RSVP True)
                    `shouldBe` [("RSVP", ["TRUE"])]
            it "omits FALSE as it's the default" $
                parameterToContent (RSVP False)
                    `shouldBe` []
    describe "SENT-BY" $ do
        context "parsing" $ do
            it "parses a SENT-BY" $
                p [("SENT-BY", [uri])] optParam1
                    `shouldBe` Just (SentBy uri')
            it "fails a doubly valued SENT-BY" $
                notP [("SENT-BY", [uri,uri])]
                     optParam1
                     (Just (SentBy uri'))
                  `shouldBe`
                    ()
            it "is unperturbed by other parameters" $
                p [("TEST", ["a"]), ("SENT-BY", [uri])] optParam1
                    `shouldBe` Just (SentBy uri')
            it "fails multiple SENT-BYs" $
                notP [("SENT-BY", [uri]), ("SENT-BY", [uri])]
                     optParam1
                     (Just (SentBy uri'))
                  `shouldBe`
                      ()
            it "is case insensitive" $
                p [("Sent-By", [uri])] optParam1
                    `shouldBe` Just (SentBy uri')
        context "printing" $
            it "prints a SENT-BY" $
                parameterToContent (SentBy uri')
                    `shouldBe` [("SENT-BY", [uri])]
    describe "TZID" $ do
        context "parsing" $
            it "parses a TZID" $
                p [("TZID", ["Europe/Oslo"])] optParam1
                    `shouldBe` Just (TZID "Europe/Oslo")
        context "printing" $
            it "prints a TZID" $
                parameterToContent (TZID "Europe/Oslo")
                    `shouldBe` [("TZID", ["Europe/Oslo"])]
    describe "VALUE" $ do
        context "parsing" $ do
            it "parses BINARY" $
                p [("VALUE", ["BINARY"])] reqParam1
                    `shouldBe` ValueTypeBinary
            it "parses BOOLEAN" $
                p [("VALUE", ["BOOLEAN"])] reqParam1
                    `shouldBe` ValueTypeBoolean
            it "parses CAL-ADDRESS" $
                p [("VALUE", ["CAL-ADDRESS"])] reqParam1
                    `shouldBe` ValueTypeCalAddress
            it "parses DATE" $
                p [("VALUE", ["DATE"])] reqParam1
                    `shouldBe` ValueTypeDate
            it "parses DATE-TIME" $
                p [("VALUE", ["DATE-TIME"])] reqParam1
                    `shouldBe` ValueTypeDateTime
            it "parses DURATION" $
                p [("VALUE", ["DURATION"])] reqParam1
                    `shouldBe` ValueTypeDuration
            it "parses FLOAT" $
                p [("VALUE", ["FLOAT"])] reqParam1
                    `shouldBe` ValueTypeFloat
            it "parses INTEGER" $
                p [("VALUE", ["INTEGER"])] reqParam1
                    `shouldBe` ValueTypeInteger
            it "parses PERIOD" $
                p [("VALUE", ["PERIOD"])] reqParam1
                    `shouldBe` ValueTypePeriod
            it "parses RECUR" $
                p [("VALUE", ["RECUR"])] reqParam1
                    `shouldBe` ValueTypeRecur
            it "parses TEXT" $
                p [("VALUE", ["TEXT"])] reqParam1
                    `shouldBe` ValueTypeText
            it "parses TIME" $
                p [("VALUE", ["TIME"])] reqParam1
                    `shouldBe` ValueTypeTime
            it "parses URI" $
                p [("VALUE", ["URI"])] reqParam1
                    `shouldBe` ValueTypeUri
            it "parses UTC-OFFSET" $
                p [("VALUE", ["UTC-OFFSET"])] reqParam1
                    `shouldBe` ValueTypeUtcOffset
            it "parses x-name" $
                p [("VALUE", ["X-CAT"])] reqParam1
                    `shouldBe` ValueTypeX "X-CAT"
            it "parses other unknown iana-token" $
                p [("VALUE", ["TRINARY"])] reqParam1
                    `shouldBe` ValueTypeX "TRINARY"
            it "fails multiple values" $
                notP [("VALUE", ["BINARY", "TEXT"])] reqParam1 ValueTypeBinary
                    `shouldBe` ()
            it "fails multiple VALUEs" $
                notP [("FBTYPE", ["BINARY"]), ("FBTYPE", ["TEXT"])]
                     reqParam1  ValueTypeBinary
                  `shouldBe` ()
        context "parsing case-insensitively" $ do
            it "parses BINARY" $
                p [("VALUE", ["binary"])] reqParam1
                    `shouldBe` ValueTypeBinary
            it "parses BOOLEAN" $
                p [("VALUE", ["boolean"])] reqParam1
                    `shouldBe` ValueTypeBoolean
            it "parses CAL-ADDRESS" $
                p [("VALUE", ["cal-ADDRESS"])] reqParam1
                    `shouldBe` ValueTypeCalAddress
            it "parses DATE" $
                p [("VALUE", ["date"])] reqParam1
                    `shouldBe` ValueTypeDate
            it "parses DATE-TIME" $
                p [("VALUE", ["date-TIME"])] reqParam1
                    `shouldBe` ValueTypeDateTime
            it "parses DURATION" $
                p [("VALUE", ["duration"])] reqParam1
                    `shouldBe` ValueTypeDuration
            it "parses FLOAT" $
                p [("VALUE", ["float"])] reqParam1
                    `shouldBe` ValueTypeFloat
            it "parses INTEGER" $
                p [("VALUE", ["integer"])] reqParam1
                    `shouldBe` ValueTypeInteger
            it "parses PERIOD" $
                p [("VALUE", ["period"])] reqParam1
                    `shouldBe` ValueTypePeriod
            it "parses RECUR" $
                p [("VALUE", ["recur"])] reqParam1
                    `shouldBe` ValueTypeRecur
            it "parses TEXT" $
                p [("VALUE", ["text"])] reqParam1
                    `shouldBe` ValueTypeText
            it "parses TIME" $
                p [("VALUE", ["time"])] reqParam1
                    `shouldBe` ValueTypeTime
            it "parses URI" $
                p [("VALUE", ["uri"])] reqParam1
                    `shouldBe` ValueTypeUri
            it "parses UTC-OFFSET" $
                p [("VALUE", ["utc-OFFSET"])] reqParam1
                    `shouldBe` ValueTypeUtcOffset
            it "parses x-name" $
                p [("VALUE", ["X-cat"])] reqParam1
                    `shouldBe` ValueTypeX "X-CAT"
            it "parses other unknown iana-token" $
                p [("VALUE", ["TRinary"])] reqParam1
                    `shouldBe` ValueTypeX "TRINARY"
        context "printing" $ do
            it "prints BINARY" $
                parameterToContent ValueTypeBinary
                    `shouldBe` [("VALUE", ["BINARY"])]
            it "prints BOOLEAN" $
                parameterToContent ValueTypeBoolean
                    `shouldBe` [("VALUE", ["BOOLEAN"])]
            it "prints CAL-ADDRESS" $
                parameterToContent ValueTypeCalAddress
                    `shouldBe` [("VALUE", ["CAL-ADDRESS"])]
            it "prints DATE" $
                parameterToContent ValueTypeDate
                    `shouldBe` [("VALUE", ["DATE"])]
            it "prints DATE-TIME" $
                parameterToContent ValueTypeDateTime
                    `shouldBe` [("VALUE", ["DATE-TIME"])]
            it "prints DURATION" $
                parameterToContent ValueTypeDuration
                    `shouldBe` [("VALUE", ["DURATION"])]
            it "prints FLOAT" $
                parameterToContent ValueTypeFloat
                    `shouldBe` [("VALUE", ["FLOAT"])]
            it "prints INTEGER" $
                parameterToContent ValueTypeInteger
                    `shouldBe` [("VALUE", ["INTEGER"])]
            it "prints PERIOD" $
                parameterToContent ValueTypePeriod
                    `shouldBe` [("VALUE", ["PERIOD"])]
            it "prints RECUR" $
                parameterToContent ValueTypeRecur
                    `shouldBe` [("VALUE", ["RECUR"])]
            it "prints TEXT" $
                parameterToContent ValueTypeText
                    `shouldBe` [("VALUE", ["TEXT"])]
            it "prints TIME" $
                parameterToContent ValueTypeTime
                    `shouldBe` [("VALUE", ["TIME"])]
            it "prints URI" $
                parameterToContent ValueTypeUri
                    `shouldBe` [("VALUE", ["URI"])]
            it "prints UTC-OFFSET" $
                parameterToContent ValueTypeUtcOffset
                    `shouldBe` [("VALUE", ["UTC-OFFSET"])]
            it "prints an x-name" $
                parameterToContent (ValueTypeX "X-CAT")
                    `shouldBe` [("VALUE", ["X-CAT"])]
            it "prints an other unknown iana-token" $
                parameterToContent (ValueTypeX "TRINARY")
                    `shouldBe` [("VALUE", ["TRINARY"])]
    describe "other parameters" $ do
        let parsed = OtherParameters $ Set.fromList
                        [ OtherParameter "X-OTHER" ["TEST", "TESTING MORE"]
                        , OtherParameter "IANA" ["VERY", "YES"]
                        ]
            unparsed = [ ("X-OTHER", ["TEST", "TESTING MORE"])
                       , ("IANA", ["VERY", "YES"])]
        context "parsing" $
            it "parses other parameters" $
                p unparsed parseOtherParameters `shouldBe` parsed
        context "printing" $
            it "prints other parameters" $
                sort (parameterPrinterToContent (printOtherParameters parsed))
                    `shouldBe` sort unparsed
