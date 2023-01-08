module Errors where

import qualified Data.Text as T
import Dhall.Core ( Expr(..) )
import Dhall.Parser ( Src, ParseError, exprFromText )

data ReadDhallError =
    RecordTypeAttributeNotRecognized T.Text
    | ExpressionNotRecognized T.Text
    deriving (Show)

data ConvError =
    DhallParseError ParseError
    | RecordLitFound Src
    | PythonObjNotFound
    deriving (Show)
