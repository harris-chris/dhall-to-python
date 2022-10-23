module ReadWrite where

import Control.Monad (join)
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.FilePath.Posix (takeDirectory)
import qualified Data.Text

import Dhall.Parser( exprFromText, ParseError )

import ExprConversion

data FileParseError = DhallError ParseError | PythonObjNotFound

dhallFileToPythonPackage :: FilePath -> FilePath -> IO ()
dhallFileToPythonPackage from_file to_folder = let
    basename = takeBaseName from_file
    in do
        parsedE <- dhallFileToPythonObj from_file
        case parsedE of
             Left err -> printErr err
             Right parsed -> writePythonObj basename to_folder parsed

printErr :: FileParseError -> IO ()
printErr err = undefined

dhallFileToPythonObj :: FilePath -> IO (Either FileParseError PythonObj)
dhallFileToPythonObj from_file = do
    contents <- TIO.readFile from_file
    let exprE = exprFromText from_file contents
    let objE = convert <$> exprE
    case objE of
        Left err -> return $ Left $ DhallError err
        Right objO -> return $ fromMaybe (Left PythonObjNotFound) objO
    --     parsedE = dhallExprToPythonObj <$> exprE
    --     in return parsedE

writePythonObj :: FilePath -> FilePath -> PythonObj -> IO ()
writePythonObj from_file to_folder output = let
    py_fname = from_file <.> "py"
    py_fpath = to_folder </> py_fname
    py_contents = T.pack $ foldl (\acc x -> acc ++ "\n\n" ++ (show x)) "" $ objs output
    init_fpath = to_folder </> "__init__.py"
    init_contents = "from ." <> (T.pack from_file) <> " import " <> (binding_name output)
    in do
        createDirectoryIfMissing False to_folder
        TIO.writeFile py_fpath py_contents
        TIO.writeFile init_fpath init_contents

