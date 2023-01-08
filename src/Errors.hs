module Errors where

import Dhall.Parser ( Src, ParseError, exprFromText )

data ReadDhallError =
    RecordTypeAttributeNotRecognized src
    | ExpressionNotRecognized src

data ConvError =
    DhallParseError ParseError
    | RecordLitFound RecordLit
    | PythonObjNotFound
    deriving (Show)
