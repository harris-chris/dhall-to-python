module ReadDhallExpr where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), Var( V ), denote )
import Dhall.Core ( Import(..), ImportHashed(..), ImportType(..), ImportMode(..) )
import Dhall.Parser ( Src, ParseError, exprFromText )

import Errors

type ObjName = T.Text
type AttributeName = T.Text

data RecordTypeAttribute =
    NaturalTypeAttribute ObjName
    | TextTypeAttribute ObjName
    | UserDefinedTypeAttribute ObjName TypeName
    deriving (Show, Eq)

data ParsedObj =
    Package ObjName [ParsedObj]
    | Record ObjName [RecordTypeAttribute]

data ParseState = ParseState {
    denote :: Bool
    , ignoreUnknown :: Bool
    , objs :: [ParsedObj]
    , errors :: [ReadDhallError]
}

includeObjE :: ParseState -> (Either ReadDhallError ParsedObj) -> ParseState
includeObjE ps (Left err) = ps { errors = errors ps ++ [err] }
includeObjE ps (Right obj) = ps { objs = objs ps ++ [obj] }

class Parses a where
    parse :: ParseState -> a -> ParseState

-- instance Converts (Expr s Void) where
--     convert _ _ = error undefined

instance Parses (Expr s a) where
    parse ps@(ParseState True _ _ _) Note s e = let
        in parse ps $ denote e
    parse ps (Let (Binding _ name _ _ _ (Record m)) e) = let
        recordE = getRecord name m
        ps' = includeObjE ps recordE
        in parse ps' e
    parse ps@(ParseState _ False _ _) Note s _ =
        ps { errors = errors ps ++ [ExpressionNotRecognized s] }

getRecord :: ObjName -> (Map T.Text (RecordField s a))-> Either ReadDhallError ParsedObj
getRecord name map = let
    attributes = sequenceA $ elems $ mapWithKey getRecordAttr map
    in Record name attributes

getRecordAttr :: T.Text -> RecordField s a -> Either ReadDhallError RecordTypeAttribute
getRecordAttr name (RecordField _ attr _ _) = getRecordTypeAttr name attr

getRecordTypeAttr :: T.Text -> Expr s a -> Either ReadDhallError RecordTypeAttribute
getRecordTypeAttr name Note _ Natural = Right $ NaturalTypeAttribute name
getRecordTypeAttr name Note _ Double = Right $ DoubleTypeAttribute name
getRecordTypeAttr name Note _ Text = Right $ TextTypeAttribute name
getRecordTypeAttr name Note _ (Var (V tn _)) = Right $ UserDefinedTypeAttribute name tn
getRecordTypeAttr name Note s _ = Left $ RecordTypeAttributeNotRecognized s
-- getDataclassTypeAttribute name (Record m) = getDataclass name m

