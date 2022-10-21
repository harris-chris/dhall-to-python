module ReadWrite where

import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.FilePath.Posix (takeDirectory)
import qualified Data.Text

import Dhall.Parser( exprFromText, ParseError )

import ParsedPackage

dhallFileToPythonPackage :: FilePath -> FilePath -> IO ()
dhallFileToPythonPackage from_file to_folder = let
    basename = takeBaseName from_file
    in do
        parsedE <- dhallFileToParsedPackage from_file
        case parsedE of
             Left err -> printErr err
             Right parsed -> writeParsedPackage basename to_folder parsed

printErr :: ParseError -> IO ()
printErr err = undefined

dhallFileToParsedPackage :: FilePath -> IO (Either ParseError ParsedPackage)
dhallFileToParsedPackage from_file = do
    contents <- TIO.readFile from_file
    let exprE = exprFromText from_file contents
        parsedE = exprToParsedPackage <$> exprE
        in return parsedE

writeParsedPackage :: FilePath -> FilePath -> ParsedPackage -> IO ()
writeParsedPackage from_file to_folder output = let
    py_fname = from_file <.> "py"
    py_fpath = to_folder </> py_fname
    py_contents = T.pack $ foldl (\acc x -> acc ++ "\n\n" ++ (show x)) "" $ objs output
    init_fpath = to_folder </> "__init__.py"
    init_contents = "from ." <> (T.pack from_file) <> " import " <> (binding_name output)
    in do
        createDirectoryIfMissing False to_folder
        TIO.writeFile py_fpath py_contents
        TIO.writeFile init_fpath init_contents

