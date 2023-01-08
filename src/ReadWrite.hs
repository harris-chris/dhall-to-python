module ReadWrite where

import Control.Monad (join)
import Data.Maybe ( fromJust, isJust )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void ( Void )
import Debug.Trace ( trace, traceShowId )
import Dhall.Core ( Expr(..) )
import Dhall.Import ( loadRelativeTo, SemanticCacheMode(..) )
import Dhall.Parser ( ParseError, Src, exprFromText )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.FilePath.Posix (takeDirectory)
import qualified Data.Text

import Dhall.Core( denote )

import Errors
import ExprConversion
import PythonPackage
import WritePythonPackage

dhallFileToDhallExpr :: FilePath -> (IO (Either DhallToPythonError (Expr Src Void)))
dhallFileToDhallExpr fromFile = do
    contents <- TIO.readFile fromFile
    let exprE = exprFromText fromFile contents
    let exprE' = denote <$> exprE
    case exprE' of
        Left err -> return $ Left $ DhallParseError err
        Right expr -> do
            let baseDir = takeDirectory fromFile
            loaded <- loadRelativeTo baseDir UseSemanticCache expr
            return $ Right loaded

dhallFileToPythonPackage :: PythonOptions -> FilePath -> FilePath -> IO ()
dhallFileToPythonPackage pyOpts fromFile toFolder = do
    dhallExprE <- dhallFileToDhallExpr fromFile
    let initConvertState = ConvertState False []
    let pythonPkgE = dhallExprE >>= (convert initConvertState)
    case pythonPkgE of
        Left err -> print err
        Right (ConvertState _ [pkg]) -> writePythonObj pyOpts toFolder 0 pkg

-- printErr :: FileParseError -> IO ()
-- printErr (DhallError err) = print "Dhall file failed to parse"
-- printErr PythonObjNotFound = print "No valid python object found"

