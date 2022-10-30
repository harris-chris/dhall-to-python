import Control.Monad (join)

import Test.Hspec
import Test.HUnit.Lang (assertFailure)
import qualified Data.Map as Map
import Debug.Trace (traceShowId)
import ExprConversion
import ReadWrite
import Dhall.Core
import Dhall.Map as DhallMap
import Dhall.Parser
import Dhall.Src
import qualified Data.Text
import qualified Data.Text.IO as TIO
import Text.Megaparsec (SourcePos(..), mkPos)
import System.FilePath ((</>), (<.>), makeRelative)
import System.Directory (listDirectory, removeDirectoryRecursive, doesFileExist)

testSourceFolder :: FilePath
testSourceFolder = "test/test_files/test_source_files"

tempOutputFolder :: FilePath
tempOutputFolder = "test/test_files/temp_output_files"

targetOutputFolder :: FilePath
targetOutputFolder = "test/test_files/target_output_files"

clearTempOutputFolder :: IO ()
clearTempOutputFolder = do
    removeList <- listDirectory tempOutputFolder
    foldl (\acc x ->
        acc >> (removeDirectoryRecursive $ tempOutputFolder </> x)
        ) (return ()) removeList

checkTempOutputAgainstTarget :: FilePath -> IO ()
checkTempOutputAgainstTarget fPath = do
    let targetFPath = targetOutputFolder </> fPath
    let tempFPath = tempOutputFolder </> fPath
    isFile <- doesFileExist targetFPath
    if isFile then
        checkFilesMatch targetFPath tempFPath
    else do
        targetFolderContents <- listDirectory targetFPath
        let contentsPlusThis = (</>) fPath <$> targetFolderContents
        let contentsRel = makeRelative targetOutputFolder <$> contentsPlusThis
        foldl (\acc x -> acc >> (checkTempOutputAgainstTarget x)) (return ()) contentsRel

checkFilesMatch :: FilePath -> FilePath -> IO ()
checkFilesMatch fileA fileB = do
    isFileA <- doesFileExist fileA
    isFileA `shouldBe` True
    isFileB <- doesFileExist fileB
    isFileB `shouldBe` True
    fileAContents <- TIO.readFile fileA
    fileBContents <- TIO.readFile fileB
    fileAContents `shouldBe` fileBContents

main :: IO ()
main = hspec $ beforeAll clearTempOutputFolder $ do
    describe "Parses dhall files" $ do
        it "Parses natural.dhall" $ do
            let fpath = testSourceFolder </> "natural.dhall" :: FilePath
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
            let fpath = testSourceFolder </> "type.dhall" :: FilePath
            contents <- TIO.readFile fpath
            let expected = Natural
            let actual = exprFromText fpath contents
            case actual of
                Left _ -> assertFailure "Expression could not be parsed"
                Right (Note src (Note src' val)) -> do
                    val `shouldBe` expected
                Right _ -> assertFailure "Parsed expression was not expected"

    describe "Convert dhall file to python file" $ do
        it "converts dataclass_only_module.dhall" $ do
            let object_name = "dataclass_only_package"
            let source = testSourceFolder </> object_name <.> "dhall"
            dhallFileToPythonPackage source $ tempOutputFolder </> object_name
            checkTempOutputAgainstTarget object_name

