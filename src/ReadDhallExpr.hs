{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE FlexibleInstances              #-}

module ReadDhallExpr where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dhall.Map
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), Var( V ), denote )
import Dhall.Core ( Import(..), ImportHashed(..), ImportType(..), ImportMode(..) )
import Dhall.Parser ( Src(..), ParseError, exprFromText )

import Errors

type ObjName = T.Text
type AttributeName = T.Text
type TypeName = T.Text

data RecordTypeAttribute =
    DoubleTypeAttribute ObjName
    | NaturalTypeAttribute ObjName
    | TextTypeAttribute ObjName
    | UserDefinedTypeAttribute ObjName TypeName
    deriving (Show, Eq)

data ParsedObj =
    PackageObj ObjName [ParsedObj]
    | RecordObj ObjName [RecordTypeAttribute]
    deriving (Show, Eq)

data ParseState = ParseState {
    ignoreUnknown :: Bool
    , objs :: [ParsedObj]
    , errors :: [ReadDhallError]
}

strictParseState = ParseState False [] []

includeObjE :: ParseState -> (Either ReadDhallError ParsedObj) -> ParseState
includeObjE ps (Left err) = ps { errors = errors ps ++ [err] }
includeObjE ps (Right obj) = ps { objs = objs ps ++ [obj] }

class Parses a where
    parse :: ParseState -> a -> ParseState

instance Parses (Expr Src a) where
    -- parse ps@(ParseState True _ _ _) (Note s e) = parse ps $ e
    parse ps (Note _ (Let (Binding _ name _ _ _ (Record m)) e)) = let
        recordE = getRecordObj name m
        ps' = includeObjE ps recordE
        in parse ps' e
    parse ps@(ParseState False _ _) (Note (Src _ _ sourceTxt) _) =
        ps { errors = errors ps ++ [ExpressionNotRecognized sourceTxt] }

getRecordObj :: ObjName -> (Map T.Text (RecordField Src a))-> Either ReadDhallError ParsedObj
getRecordObj name map = let
    attributes = sequenceA $ elems $ mapWithKey getRecordAttr map
    in (RecordObj name ) <$> attributes

getRecordAttr :: T.Text -> RecordField Src a -> Either ReadDhallError RecordTypeAttribute
getRecordAttr name (RecordField _ attr _ _) = getRecordTypeAttr name attr

getRecordTypeAttr :: T.Text -> Expr Src a -> Either ReadDhallError RecordTypeAttribute
getRecordTypeAttr name (Note _ Natural) = Right $ NaturalTypeAttribute name
getRecordTypeAttr name (Note _ Double) = Right $ DoubleTypeAttribute name
getRecordTypeAttr name (Note _ Text) = Right $ TextTypeAttribute name
getRecordTypeAttr name (Note _ (Var (V tn _))) = Right $ UserDefinedTypeAttribute name tn
getRecordTypeAttr name (Note (Src _ _ srcText) _) = Left $ RecordTypeAttributeNotRecognized srcText

-- dhallExprToParsedObj :: FilePath -> (IO (Either DhallToPythonError (Expr Src Void)))
-- dhallFileToDhallExpr fromFile = do
--     contents <- TIO.readFile fromFile
--     let exprE = exprFromText fromFile contents
--     let exprE' = denote <$> exprE
--     case exprE' of
--         Left err -> return $ Left $ DhallParseError err
--         Right expr -> do
--             let baseDir = takeDirectory fromFile
--             loaded <- loadRelativeTo baseDir UseSemanticCache expr
--             return $ Right loaded

