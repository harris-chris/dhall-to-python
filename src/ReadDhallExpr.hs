{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE FlexibleInstances              #-}

module ReadDhallExpr where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Debug.Trace ( trace, traceShowId )
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

parsedFromExprE :: IO (Either ParseError (Expr Void Import)) -> IO Parsed -> IO Parsed
parsedFromExprE exprEIO psIO = do
    exprE <- exprEIO
    case exprE of
         Left err -> do
             ps <- psIO
             let newErr = DhallParseError err
             return ps { errs = newErr:(errs ps) }
         Right expr ->
             parseIO psIO expr
             -- let exprDenoted = denote expr :: Expr s Import
             -- in parseIO psIO (exprDenoted :: Expr s Import)

includeObjE :: Parsed -> (Either ReadDhallError ParsedObj) -> Parsed
includeObjE ps (Left err) = ps { errs = err:(errs ps) }
includeObjE ps (Right obj) = ps { objs = obj:(objs ps) }

class ParsesIO a where
    parseIO :: IO Parsed -> a -> IO Parsed

instance ParsesIO (Expr Void Import) where
    parseIO psIO (Let (Binding _ name _ _ _ (Embed (impt))) e) =
        let psIO' = addImport psIO name impt
        in trace "parsing import" $ parseIO psIO' e
    parseIO psIO (Let (Binding _ name _ _ _ (Record m)) e) =
        let psIO' = addRecordObj name m <$> psIO
        in trace "parsing record" $ parseIO psIO' e
    parseIO psIO (Let (Binding _ name _ _ _ (RecordLit m)) (Var _ )) =
        trace "parsing package" $ addPackageObj name m <$> psIO
    -- parseIO psIO (
    --     Let (Binding _ name _ _ _ (
    --         Embed (Import (ImportHashed hash (Local prefix file)) Code))
    --     ) e ) =
    --         let path = show file
    --         in readParsedFromFile psIO path
    -- If it's not an IO-related expression, use the non-IO parse
    parseIO psIO expr = do
        ps <- (trace "parsing unknown" psIO)
        if ignoreUnknown ps then
            return ps
        else
            let showOpts = ShowOptions True (Just (20, 10))
            in return $ addExprNotRecognized expr showOpts ps

addExprNotRecognized :: Expr Void Import -> ShowOptions -> Parsed -> Parsed
addExprNotRecognized expr showOpts ps =
    let exprErr = ExprNotRecognized $ showExpr showOpts expr
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
addImport psIO name (Import (ImportHashed hash (Local prefix file)) Code) =
    let path = show file
    in readParsedFromFile psIO path

readParsedFromFile :: IO Parsed -> FilePath -> IO Parsed
readParsedFromFile psIO fpath =
    -- IO contents
    -- IO (Either ParseError, expr)
    -- IO ( if err then this state, if expr then parse it)
    let psNewIO = (\ps -> ps { errs = [], objs = [] }) <$> psIO
        contentsIO = TIO.readFile fpath
        exprEIO = exprFromText fpath <$> contentsIO
        exprEIO' = denote <$> exprEIO
        psNewIO' = parsedFromExprE exprEIO' psNewIO
    in mergePackageIntoParsed fpath <$> psIO <*> psNewIO'

mergePackageIntoParsed :: FilePath -> Parsed -> Parsed -> Parsed
mergePackageIntoParsed fp (Parsed i os es d) (Parsed _ [pk@(PackageObj _ _ _)] ers _) =
    let pkgFinal = traceShowId (finalizeObj pk)
    in Parsed i (pkgFinal:os) (ers ++ es) d
mergePackageIntoParsed fp (Parsed i os es d) (Parsed _ [] ers _) =
    Parsed i os (ers ++ es) d
mergePackageIntoParsed fp (Parsed i os es d) (Parsed _ (o:objs) ers _) =
    let err = UnableToImportPackage fp
    in Parsed i os (ers ++ es) d

