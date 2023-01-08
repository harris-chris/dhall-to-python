module Errors where

import Dhall.Parser ( Src, ParseError, exprFromText )

data DhallToPythonError = DhallParseError ParseError | PythonObjNotFound
    deriving (Show)
