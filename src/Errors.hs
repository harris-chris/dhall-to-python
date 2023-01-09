module Errors where

import qualified Data.Text as T
import Dhall.Core ( Expr(..) )
import Dhall.Parser ( Src, ParseError, exprFromText )

type Source = T.Text
type Expression = T.Text

data ReadDhallError =
    DhallParseError ParseError
    | RecordTypeAttrNotRecognized Expression
    | ExpressionNotRecognized Expression

instance Show ReadDhallError where
    show (RecordTypeAttrNotRecognized e) =
        "RecordTypeAttrNotRecognized: " ++ T.unpack e
    show (ExpressionNotRecognized e) =
        "ExpressionNotRecognized:\n" ++ T.unpack e
    show (DhallParseError e) = show e

