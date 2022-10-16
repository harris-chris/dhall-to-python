import Test.Hspec
import Test.HUnit.Lang (assertFailure)
import Debug.Trace (traceShowId)
import DhallToPythonLib
import Dhall.Core
import Dhall.Parser
import Dhall.Src
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec (SourcePos(..), mkPos)
import System.FilePath ((</>))

testData :: FilePath
testData = "test/test_files"

main :: IO ()
main = hspec $ do
    describe "Parse basic dhall expressions" $ do
        it "parses a natural number to a dhall Natural" $ do
            let expected = NaturalLit 21 :: Expr Src Import
            let actual = exprFromText "Simple Natural" "21" :: Either ParseError (Expr Src Import)
            case actual of
                Left err -> assertFailure "Could not parse"
                Right ( Note _ val ) -> val `shouldBe` expected
                Right _ -> assertFailure "Expected not actual"

    describe "Parses dhall files" $ do
        it "Parses natural.dhall" $ do
            let fpath = testData </> "natural.dhall" :: FilePath
            contents <- TIO.readFile fpath
            let expected = NaturalLit 22
            let actual = exprFromText fpath contents
            case actual of
                Left _ -> assertFailure "Expression could not be parsed"
                Right (Note src (Note src' val)) -> do
                    -- The original source, as it was read
                    src `shouldBe` Src
                        (SourcePos fpath (mkPos 1) (mkPos 1))
                        (SourcePos fpath (mkPos 2) (mkPos 1))
                        "22\n"
                    -- The parsed source, with eg newlines removed
                    src' `shouldBe` Src
                        (SourcePos fpath (mkPos 1) (mkPos 1))
                        (SourcePos fpath (mkPos 1) (mkPos 3))
                        "22"
                    val `shouldBe` expected
                Right _ -> assertFailure "Parsed expression was not expected"

        it "parses type.dhall" $ do
            let fpath = testData </> "type.dhall" :: FilePath
            contents <- TIO.readFile fpath
            let expected = Natural
            let actual = exprFromText fpath contents
            case actual of
                Left _ -> assertFailure "Expression could not be parsed"
                Right (Note src (Note src' val)) -> do
                    val `shouldBe` expected
                Right _ -> assertFailure "Parsed expression was not expected"

        it "parses untyped_record.dhall" $ do
            let fpath = testData </> "untyped_record.dhall" :: FilePath
            contents <- TIO.readFile fpath
            let expected = Natural
            let actual = exprFromText fpath contents
            case actual of
                Left _ -> assertFailure "Expression could not be parsed"
                Right (Note src (Note src' val)) -> do
                    val `shouldBe` expected
                Right _ -> assertFailure "Parsed expression was not expected"

    describe "Performs Dhall to Python conversions" $ do
        it "Converts " $ do
            let fpath = testData </> "natural.dhall" :: FilePath
            contents <- TIO.readFile fpath
            let expected = NaturalLit 22
            let actual = exprFromText fpath contents
            case actual of
                Left _ -> assertFailure "Expression could not be parsed"
                Right (Note src (Note src' val)) -> do
                    -- The original source, as it was read
                    src `shouldBe` Src
                        (SourcePos fpath (mkPos 1) (mkPos 1))
                        (SourcePos fpath (mkPos 2) (mkPos 1))
                        "22\n"
                    -- The parsed source, with eg newlines removed
                    src' `shouldBe` Src
                        (SourcePos fpath (mkPos 1) (mkPos 1))
                        (SourcePos fpath (mkPos 1) (mkPos 3))
                        "22"
                    val `shouldBe` expected
                Right _ -> assertFailure "Parsed expression was not expected"

