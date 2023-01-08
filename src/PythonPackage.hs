{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE FlexibleInstances              #-}

module PythonPackage where

import Debug.Trace ( trace, traceShowId )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe ( catMaybes )
import Data.Typeable ( typeOf )
import Data.Void ( Void )
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), Var( V ), denote )
import Dhall.Core ( Import(..), ImportHashed(..), ImportType(..), ImportMode(..) )
import Dhall.Core ( FilePrefix(..), File(..) )
import Dhall.Map as DhallMap
import Dhall.Parser ( Src, ParseError, exprFromText )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), (<.>), takeBaseName )
import Text.Casing ( quietSnake )

data DataclassOptions = DataclassOptions {
    frozen :: Bool
    , eq :: Bool
}

getDataclassParameters :: DataclassOptions -> T.Text
getDataclassParameters opts = let
    argsO = [
        if (frozen opts) then Just "frozen=True" else Nothing
        , if (eq opts) then Just "eq=True" else Nothing
        ]
    args = catMaybes argsO
    in if length args == 0
        then ""
        else "(" <> (T.intercalate ", " args) <> ")"

defaultDataclassOptions :: DataclassOptions
defaultDataclassOptions = DataclassOptions True True

data PythonOptions = PythonOptions {
    dataclassOptions :: DataclassOptions
}

defaultPythonOptions :: PythonOptions
defaultPythonOptions = PythonOptions defaultDataclassOptions

type AttributeName = T.Text

data PythonObj =
    DataclassFloatAttribute AttributeName
    | DataclassIntAttribute AttributeName
    | DataclassStrAttribute AttributeName
    | DataclassUserDefinedTypeAttribute AttributeName T.Text
    | PythonDataclass AttributeName [PythonObj]
    | PythonPackage AttributeName [PythonObj]
    deriving (Eq, Show)

