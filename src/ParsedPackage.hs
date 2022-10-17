module ParsedPackage where

import Data.Text

import Dhall.Core ( Expr(..), RecordField(..) )
import ExprConversion

data ParsedPackage = ParsedPackage {
    binding_name :: Text
    , objs :: [PythonObj]
    , packages :: [ParsedPackage]
} deriving (Show, Eq)

exprToParsedPackage :: Expr s a -> ParsedPackage
exprToParsedPackage = undefined

