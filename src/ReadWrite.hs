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

dhallFileToPythonPackage :: FilePath -> FilePath -> IO (Either ParseError ())
dhallFileToPythonPackage from_file to_folder = let
    basename = takeBaseName from_file
    in join $ writeParsedPackage basename to_folder <$> dhallFileToParsedPackage from_file
    -- parsedIO = dhallFileToParsedPackage from_file
    -- -- We want to go IO (Either ParseError ParsedPackage) to IO (Either ParseError ())
    -- -- Our write function returns IO()
    -- -- so is this not writeParsedPackage from_file to_folder fmap . >>=
    -- in do
    --     parsed <- dhallFileToParsedPackage from_file :: Either ParseError ParsedPackage
    --     writeParsedPackage basename to_folder  parsed

dhallFileToParsedPackage :: FilePath -> IO (Either ParseError ParsedPackage)
dhallFileToParsedPackage from_file = do
    contents <- TIO.readFile from_file
    let expr = exprFromText from_file contents
        in return $ exprToParsedPackage <$> expr

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

