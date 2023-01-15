{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE FlexibleInstances              #-}

module ReadDhallExpr where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Debug.Trace ( trace, traceShowId )
import System.Directory ( getCurrentDirectory, makeAbsolute )
import System.FilePath
import Dhall.Map
import Dhall.Core ( FilePrefix(..), File(..), Directory(..) )
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), Var( V ) )
import Dhall.Core ( Import(..), ImportHashed(..), ImportType(..), ImportMode(..) )
import Dhall.Core ( censorExpression, denote, pretty )
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
finalizeObj (PackageObj name objs exp) = PackageObj name objs exp
finalizeObj (RecordObj name attrs) = RecordObj name attrs

data Parsed = Parsed {
        ignoreUnknown :: Bool
        , objs :: [ParsedObj]
        , errs :: [ReadDhallError]
        , filepath :: FilePath
    }
    deriving (Eq, Show)

getStrictParsed :: FilePath -> Parsed
getStrictParsed fpath = Parsed False [] [] fpath

emptyParsed :: Parsed -> Parsed
emptyParsed ps = ps { errs = [], objs = [] }

updateFilePath :: FilePath -> Parsed -> Parsed
updateFilePath fpath ps =
    if isRelative fpath then
        let currentDir = dropFileName $ trace ("current filepath " ++ filepath ps) (filepath ps)
        in ps { filepath = (trace ("Current dir is " ++ currentDir) currentDir) </> fpath }
    else
        ps { filepath = fpath }

parsedFromExprE :: IO (Either ParseError (Expr Void Import)) -> IO Parsed -> IO Parsed
parsedFromExprE exprEIO psIO = do
    exprE <- exprEIO
    case exprE of
         Left err -> do
             ps <- psIO
             let newErr = DhallParseError err
             return $ ps { errs = newErr:(errs ps) }
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
addPackageObj name map (Parsed i objs errs fp) =
    let pkg = PackageObj name objs (keys map)
        pkg' = finalizeObj pkg
    in Parsed i [pkg'] errs fp

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
    let psNewIO = do
            ps <- psIO
            putStrLn $ filepath ps
            let absPath = getAbsoluteFilePath prefix file (filepath ps)
            let ps' = (emptyParsed . updateFilePath absPath ) ps
            readParsedFromFile (return ps') (trace ("absPath is " ++ absPath) absPath)
    in mergePackageIntoParsed (T.unpack $ pretty file) <$> psIO <*> psNewIO

getAbsoluteFilePath :: FilePrefix -> File -> FilePath -> FilePath
getAbsoluteFilePath Absolute file currentIO =
    dhallFileToFilePath file
getAbsoluteFilePath Here file current =
    let currentDir = takeDirectory current
        fileName = dhallFileToFilePath file
    in (trace ("Rel to " ++ currentDir) currentDir) </> fileName
getAbsoluteFilePath Parent file current =
    let parentDir = takeDirectory . takeDirectory $ current
    in parentDir </> (dhallFileToFilePath file)
getAbsoluteFilePath Home file current = error "Cannot import relative to $HOME"

dhallFileToFilePath :: File -> FilePath
dhallFileToFilePath (File (Directory comps) file) =
    let dir = joinPath $ reverse $ T.unpack <$>  comps
    in dir </> (T.unpack file)

strictReadParsedFromFile :: FilePath -> IO Parsed
strictReadParsedFromFile fpath =
    let ps = getStrictParsed fpath
        ps' = updateFilePath fpath ps
        psIO = do return ps'
    in readParsedFromFile psIO fpath

readParsedFromFileIO :: IO Parsed -> IO FilePath -> IO Parsed
readParsedFromFileIO psIO fpathIO =
    let exprEIO = do
            ps <- psIO
            fpath <- fpathIO
            contents <- TIO.readFile fpath
            let exprE = (exprFromText fpath contents)
            return $ denote <$> exprE
    in parsedFromExprE exprEIO psIO

readParsedFromFile :: IO Parsed -> FilePath -> IO Parsed
readParsedFromFile psIO fpath =
    let fpathIO = do return fpath
    in readParsedFromFileIO psIO fpathIO

mergePackageIntoParsed :: FilePath -> Parsed -> Parsed -> Parsed
mergePackageIntoParsed fp (Parsed i os es f) (Parsed _ [pk@(PackageObj _ _ _)] ers _) =
    let pkgFinal = traceShowId (finalizeObj pk)
    in Parsed i (pkgFinal:os) (ers ++ es) f
mergePackageIntoParsed fp (Parsed i os es f) (Parsed _ [] ers _) =
    Parsed i os (ers ++ es) f
mergePackageIntoParsed fp (Parsed i os es f) (Parsed _ (o:objs) ers _) =
    let err = UnableToImportPackage fp
    in Parsed i os (ers ++ es) f

