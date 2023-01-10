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
    deriving (Eq, Show)

data ParsedObj =
    PackageObj ObjName [ParsedObj] [ObjName] -- [All objs] [Exposed object names]
    | RecordObj ObjName [RecordTypeAttr]
    deriving (Eq, Show)

finalizeObj :: ParsedObj -> ParsedObj
finalizeObj (PackageObj name objs exp) = PackageObj name (reverse objs) (reverse exp)
finalizeObj (RecordObj name attrs) = RecordObj name attrs

data Parsed = Parsed {
        ignoreUnknown :: Bool
        , objs :: [ParsedObj]
        , errs :: [ReadDhallError]
        , deNoted :: Bool
    }
    deriving (Eq, Show)

strictParsed = Parsed False [] [] False

includeObjE :: Parsed -> (Either ReadDhallError ParsedObj) -> Parsed
includeObjE ps (Left err) = ps { errs = err:(errs ps) }
includeObjE ps (Right obj) = ps { objs = obj:(objs ps) }

class ParsesIO a where
    parseIO :: IO Parsed -> a -> IO Parsed

instance (Show s) => ParsesIO (Expr s Import) where
    parseIO psIO (Let (Binding _ name _ _ _ (Embed (impt))) e) =
        let psIO' = addImport psIO name impt
        in parseIO psIO' e
    parseIO psIO (Let (Binding _ name _ _ _ (Record m)) e) =
        let psIO' = addRecordObj name m <$> psIO
        in parseIO psIO' e
    parseIO psIO (Let (Binding _ name _ _ _ (RecordLit m)) (Var _ )) =
        addPackageObj name m <$> psIO
    -- parseIO psIO (
    --     Let (Binding _ name _ _ _ (
    --         Embed (Import (ImportHashed hash (Local prefix file)) Code))
    --     ) e ) =
    --         let path = show file
    --         in readParsedFromFile psIO path
    -- If it's not an IO-related expression, use the non-IO parse
    parseIO psIO @(Parsed False _ _ _) expr =
        let showOpts = ShowOptions True (Just (20, 10))
        in addExpressionNotRecognizedError expr showOpts <$> psIO

addExpressionNotRecognizedError :: Expr s a -> ShowOptions -> Parsed -> Parsed
addExpressionNotRecognizedError expr showOpts ps =
    let exprErr = ExpressionNotRecognized $ showExpr showOpts expr
    in ps { errs = exprErr:(errs ps) }

addRecordObj :: (Show a, Show s) => ObjName -> (Map T.Text (RecordField s a)) -> Parsed -> Parsed
addRecordObj name map ps =
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

addPackageObj :: T.Text -> (Map T.Text (RecordField s a)) -> Parsed ->  Parsed
-- only include the objects in the map? Need to include all objects, but only expose the ones in the map
-- Note that there can only be one package per file and it has to be at the base level
-- Think about how to handle python named imports
addPackageObj name map (Parsed i objs errs dn) =
    let pkg = PackageObj name objs (keys map)
        pkg' = finalizeObj pkg
    in Parsed i [pkg'] errs dn

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

addImport :: IO Parsed -> T.Text -> Import -> IO Parsed
-- addImport psIO name (Import (ImportHashed hash (Local prefix file)) Code) = undefined
addImport psIO name (Import (ImportHashed hash (Local prefix file)) Code) =
    let path = show file
    in readParsedFromFile psIO path

readParsedFromFile :: IO Parsed -> FilePath -> IO Parsed
readParsedFromFile psIO fpath = do
    ps <- psIO
    contents <- TIO.readFile fpath
    let exprE = exprFromText fpath contents
    let psNew = strictParsed
    let psNew' = case exprE of
                      Left e -> psNew { errs = [ DhallParseError e ] }
                      Right expr -> (parse . denote) psNew expr
    -- Don't think this next line is going to work because the sub-modules
    -- will put everything that's already loaded in their own package.
    -- We originally thought to work from the other direction.
    -- But could do it this way. Just need to make it the rule that
    -- everything that gets added, gets added to a pre-existing base package
    return $ mergePackageIntoParsed fpath ps psNew'

mergePackageIntoParsed :: FilePath -> Parsed -> Parsed -> Parsed
mergePackageIntoParsed fp (Parsed i os es d) (Parsed _ [pk@(PackageObj _ _ _)] ers _) =
    let pkgFinal = finalizeObj pk
    in Parsed i (pkgFinal:os) (ers ++ es) d
mergePackageIntoParsed fp (Parsed i os es d) (Parsed _ [] ers _) =
    Parsed i os (ers ++ es) d
mergePackageIntoParsed fp (Parsed i os es d) (Parsed _ (o:objs) ers _) =
    let err = UnableToImportPackage fp
    in Parsed i os (ers ++ es) d

