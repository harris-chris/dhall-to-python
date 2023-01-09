{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE FlexibleInstances              #-}

module ReadDhallExpr where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Dhall.Map
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), Var( V ) )
import Dhall.Core ( Import(..), ImportHashed(..), ImportType(..), ImportMode(..) )
import Dhall.Core ( censorExpression, denote )
import Dhall.Parser ( Src(..), ParseError, exprFromText )

-- import DhallExprUtils (getTypesOnly)
import Errors

type ObjName = T.Text
type AttrName = T.Text
type TypeName = T.Text

data RecordTypeAttr =
    DoubleTypeAttribute ObjName
    | NaturalTypeAttribute ObjName
    | TextTypeAttribute ObjName
    | UserDefinedTypeAttribute ObjName TypeName
    deriving (Show, Eq)

data ParsedObj =
    PackageObj ObjName [ParsedObj] [ObjName] -- [All objs] [Exposed object names]
    | RecordObj ObjName [RecordTypeAttr]
    deriving (Show, Eq)

data ParseState = ParseState {
    ignoreUnknown :: Bool
    , objs :: [ParsedObj]
    , errs :: [ReadDhallError]
}

strictParseState = ParseState False [] []

includeObjE :: ParseState -> (Either ReadDhallError ParsedObj) -> ParseState
includeObjE ps (Left err) = ps { errs = errs ps ++ [err] }
includeObjE ps (Right obj) = ps { objs = objs ps ++ [obj] }

class Parses a where
    parse :: ParseState -> a -> ParseState

-- The central function here; most others are called from this
instance (Show a) => Parses (Expr Src a) where
    parse ps (Note _ (Let (Binding _ name _ _ _ (Record m)) e)) =
        let ps' = addRecordObj ps name m
        in parse ps' e
    -- Note that we can only parse a package if it's the final obj in the expression
    parse ps (
        Note _ (Let (Binding _ name _ _ _ (RecordLit m)) (Var _ ))
        ) =
        addPackageObj ps name m
    parse ps@(ParseState False _ _) expr@(Note (Src _ _ srcTxt) _) =
        let exprTypesOnly = T.pack $ show $ expr
        in ps { errs = errs ps ++ [ExpressionNotRecognized srcTxt exprTypesOnly] }

addRecordObj :: ParseState -> ObjName -> (Map T.Text (RecordField Src a)) -> ParseState
addRecordObj ps name map =
    let attributes = sequenceA $ elems $ mapWithKey getRecordAttr map
        objE = (RecordObj name ) <$> attributes
    in includeObjE ps objE

getRecordAttr :: AttrName -> RecordField Src a -> Either ReadDhallError RecordTypeAttr
getRecordAttr name (RecordField _ attr _ _) = getRecordTypeAttr name attr

getRecordTypeAttr :: AttrName -> Expr Src a -> Either ReadDhallError RecordTypeAttr
getRecordTypeAttr name (Note _ Natural) = Right $ NaturalTypeAttribute name
getRecordTypeAttr name (Note _ Double) = Right $ DoubleTypeAttribute name
getRecordTypeAttr name (Note _ Text) = Right $ TextTypeAttribute name
getRecordTypeAttr name (Note _ (Var (V tn _))) = Right $ UserDefinedTypeAttribute name tn
getRecordTypeAttr name (Note (Src _ _ txt) _) = Left $ RecordTypeAttrNotRecognized txt

addPackageObj :: ParseState -> T.Text -> (Map T.Text (RecordField Src a)) -> ParseState
-- only include the objects in the map? Need to include all objects, but only expose the ones in the map
-- Note that there can only be one package per file and it has to be at the base level
-- Think about how to handle python named imports
addPackageObj (ParseState i objs errs) name map =
    let pkg = PackageObj name (reverse objs) (keys map)
    in ParseState i [pkg] errs

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

