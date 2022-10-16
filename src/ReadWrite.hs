module ReadWrite where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.FilePath.Posix (takeDirectory)
import qualified Data.Text
import DhallToPythonLib

dhallFileToPythonPackage :: FilePath -> FilePath -> IO ()
dhallFileToPythonPackage from_file to_folder = let
    basename = takeBaseName from_file
    in do
        parsed <- dhallFileToParsedPackage from_file
        writeParsedPackage basename to_folder parsed

dhallFileToParsedPackage :: FilePath -> IO ParsedPackage
dhallFileToParsedPackage from_file = undefined

writeParsedPackage :: FilePath -> FilePath -> ParsedPackage -> IO ()
writeParsedPackage basename to_folder output = let
    py_fname = basename <.> "py"
    py_fpath = to_folder </> py_fname
    py_contents = T.pack $ foldl (\acc x -> acc ++ "\n\n" ++ (show x)) "" $ objs output
    init_fpath = to_folder </> "__init__.py"
    init_contents = "from ." <> (T.pack basename) <> " import " <> (binding_name output)
    in do
        createDirectoryIfMissing False to_folder
        TIO.writeFile py_fpath py_contents
        TIO.writeFile init_fpath init_contents

