module Errors where

import qualified Data.Text as T
import Dhall.Core ( Expr(..) )
import Dhall.Parser ( Src, ParseError(..), exprFromText )

type Source = T.Text
type Expression = T.Text

data ReadDhallError =
    DhallParseError ParseError
    | RecordTypeAttrNotRecognized Expression
    | ExprNotRecognized Expression
    | UnableToImportPackage FilePath
    deriving (Eq)

instance Show ReadDhallError where
    show (RecordTypeAttrNotRecognized e) =
        "RecordTypeAttrNotRecognized:\n" ++ T.unpack e
    show (ExprNotRecognized e) =
        "ExprNotRecognized:\n" ++ T.unpack e
    show (DhallParseError e) = show e

instance Eq ParseError where
    a == b = input a == input b

