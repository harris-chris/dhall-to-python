module ReadWrite where

import Control.Monad (join)
import Data.Maybe ( fromJust, isJust )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace ( trace, traceShowId )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.FilePath.Posix (takeDirectory)
import qualified Data.Text

import Dhall.Core( denote )
import Dhall.Parser( exprFromText, ParseError )

import ExprConversion

data FileParseError = DhallError ParseError | PythonObjNotFound

dhallFileToPythonPackage :: PythonOptions -> FilePath -> FilePath -> IO ()
dhallFileToPythonPackage pyOpts fromFile toFolder = do
    parsedE <- dhallFileToPythonPackageObj fromFile
    case parsedE of
         Left err -> printErr err
         Right parsed -> writePythonObj pyOpts toFolder 0 parsed

printErr :: FileParseError -> IO ()
printErr (DhallError err) = print "Dhall file failed to parse"
printErr PythonObjNotFound = print "No valid python object found"

dhallFileToPythonPackageObj :: FilePath -> IO (Either FileParseError PythonObj)
dhallFileToPythonPackageObj fromFile = do
    contents <- TIO.readFile fromFile
    let exprE = exprFromText fromFile contents
    let exprE' = denote <$> exprE
    let cs = newConvertState
    let objE = convert cs <$> exprE'
    case objE of
        Left err -> return $ Left $ DhallError err
        Right (ConvertState d objs) -> case objs of
            [pkg] -> return (Right pkg)
            _ -> return (Left PythonObjNotFound)
            -- then return (Right (fromJust objO))
            -- else return (Left PythonObjNotFound)

