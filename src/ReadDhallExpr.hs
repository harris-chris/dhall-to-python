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

import DhallExprUtils (ShowOptions(..), showExpr)

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
    , deNoted :: Bool
}

strictParseState = ParseState False [] [] False

includeObjE :: ParseState -> (Either ReadDhallError ParsedObj) -> ParseState
includeObjE ps (Left err) = ps { errs = errs ps ++ [err] }
includeObjE ps (Right obj) = ps { objs = objs ps ++ [obj] }

class Parses a where
    parse :: ParseState -> a -> ParseState

-- The central function here; most others are called from this
instance (Show a, Show s) => Parses (Expr s a) where
    parse ps@(ParseState _ _ _ False) e =
        let ps' = ps { deNoted = True }
            deNotedExpr = denote e :: Expr s a
        in parse ps' deNotedExpr
    parse ps (Let (Binding _ name _ _ _ (Record m)) e) =
        let ps' = addRecordObj ps name m
        in parse ps' e
    -- Note that we can only parse a package if it's the final obj in the expression
    parse ps (Let (Binding _ name _ _ _ (RecordLit m)) (Var _ )) =
        addPackageObj ps name m
    parse ps@(ParseState False _ _ _) expr =
        let showOpts = ShowOptions True (Just (20, 10))
            exprErr = ExpressionNotRecognized $ showExpr showOpts expr
        in ps { errs = errs ps ++ [ exprErr ] }

addRecordObj :: (Show a, Show s) => ParseState -> ObjName -> (Map T.Text (RecordField s a)) -> ParseState
addRecordObj ps name map =
    let attributes = sequenceA $ elems $ mapWithKey getRecordAttr map
        objE = (RecordObj name ) <$> attributes
    in includeObjE ps objE

getRecordAttr :: (Show a, Show s) => AttrName -> RecordField s a -> Either ReadDhallError RecordTypeAttr
getRecordAttr name (RecordField _ attr _ _) = getRecordTypeAttr name attr

getRecordTypeAttr :: (Show a, Show s) => AttrName -> Expr s a -> Either ReadDhallError RecordTypeAttr
getRecordTypeAttr name Natural = Right $ NaturalTypeAttribute name
getRecordTypeAttr name Double = Right $ DoubleTypeAttribute name
getRecordTypeAttr name Text = Right $ TextTypeAttribute name
getRecordTypeAttr name (Var (V tn _)) = Right $ UserDefinedTypeAttribute name tn
getRecordTypeAttr name expr =
    let showOpts = ShowOptions True (Just (60, 0))
        exprTxt = showExpr showOpts expr
    in Left $ RecordTypeAttrNotRecognized exprTxt

addPackageObj :: ParseState -> T.Text -> (Map T.Text (RecordField s a)) -> ParseState
-- only include the objects in the map? Need to include all objects, but only expose the ones in the map
-- Note that there can only be one package per file and it has to be at the base level
-- Think about how to handle python named imports
addPackageObj (ParseState i objs errs dn) name map =
    let pkg = PackageObj name (reverse objs) (keys map)
    in ParseState i [pkg] errs dn

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

