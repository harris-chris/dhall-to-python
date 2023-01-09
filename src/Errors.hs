module Errors where

import qualified Data.Text as T
import Dhall.Core ( Expr(..) )
import Dhall.Parser ( Src, ParseError, exprFromText )

type Source = T.Text
type Expression = T.Text

data ReadDhallError =
    RecordTypeAttrNotRecognized T.Text
    | ExpressionNotRecognized Source Expression
    deriving (Show)

data ConvError =
    DhallParseError ParseError
    | RecordLitFound Src
    | PythonObjNotFound
    deriving (Show)
