{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE FlexibleInstances              #-}

module ExprConversion where

import Debug.Trace ( trace, traceShowId )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe ( catMaybes )
import Data.Typeable ( typeOf )
import Data.Void ( Void )
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), Var( V ), denote )
import Dhall.Core ( Import(..), ImportHashed(..), ImportType(..), ImportMode(..) )
import Dhall.Core ( FilePrefix(..), File(..) )
import Dhall.Map as DhallMap
import Dhall.Parser ( Src, ParseError, exprFromText )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), (<.>), takeBaseName )
import Text.Casing ( quietSnake )

data DataclassOptions = DataclassOptions {
    frozen :: Bool
    , eq :: Bool
}

getDataclassParameters :: DataclassOptions -> T.Text
getDataclassParameters opts = let
    argsO = [
        if (frozen opts) then Just "frozen=True" else Nothing
        , if (eq opts) then Just "eq=True" else Nothing
        ]
    args = catMaybes argsO
    in if length args == 0
        then ""
        else "(" <> (T.intercalate ", " args) <> ")"

defaultDataclassOptions :: DataclassOptions
defaultDataclassOptions = DataclassOptions True True

data PythonOptions = PythonOptions {
    dataclassOptions :: DataclassOptions
}

defaultPythonOptions :: PythonOptions
defaultPythonOptions = PythonOptions defaultDataclassOptions

data ConvertState = ConvertState {
    isDenoted :: Bool
    , objs :: [PythonObj]
}

newConvertState = ConvertState False []

type AttributeName = T.Text

data PythonObj =
    DataclassFloatAttribute AttributeName
    | DataclassIntAttribute AttributeName
    | DataclassStrAttribute AttributeName
    | DataclassUserDefinedTypeAttribute AttributeName T.Text
    | PythonDataclass AttributeName [PythonObj]
    | PythonPackage AttributeName [PythonObj]
    deriving (Eq, Show)

writePythonObj :: PythonOptions -> FilePath -> Int -> PythonObj -> IO ()
writePythonObj pyOpts baseFolder indent (PythonPackage name objs) = let
    snakeName = quietSnake (T.unpack (trace ("pkg name is " ++ T.unpack name) name))
    pyFname = (trace ("snakename is " ++ snakeName) snakeName) <.> "py"
    pyFpath = baseFolder </> pyFname
    initFpath = baseFolder </> "__init__.py"
    initContents = T.pack $ "from ." <> snakeName <> " import " <> snakeName
    pyHeader = "from dataclasses import dataclass\n\n"
    in do
        createDirectoryIfMissing False baseFolder
        TIO.writeFile initFpath initContents
        TIO.writeFile pyFpath pyHeader
        foldl (\io x -> io
            >> writePythonObj pyOpts pyFpath indent x
            >> appendFile pyFpath "\n"
            ) (return ()) objs

writePythonObj pyOpts toFile indent (PythonDataclass name objs) = let
    dataclassParams = getDataclassParameters $ dataclassOptions pyOpts
    writeStr =
        "@dataclass" <> dataclassParams <> "\n" <>
        "class " <> name <> ":\n"
    in do
        TIO.appendFile toFile (trace ("writing " ++ T.unpack writeStr) writeStr)
        foldl
            (\io x -> io >> writePythonObj pyOpts toFile (indent + 1) x)
            (return ())
            objs

writePythonObj pyOpts toFile indent (DataclassFloatAttribute name) = let
    writeStr = getIndent indent <> name <> ": float\n"
    in TIO.appendFile toFile writeStr

writePythonObj pyOpts toFile indent (DataclassIntAttribute name) = let
    writeStr = getIndent indent <> name <> ": int\n"
    in TIO.appendFile toFile writeStr

writePythonObj pyOpts toFile indent (DataclassStrAttribute name) = let
    writeStr = getIndent indent <> name <> ": str\n"
    in TIO.appendFile toFile writeStr

writePythonObj pyOpts toFile indent (DataclassUserDefinedTypeAttribute key typeName) = let
    writeStr = getIndent indent <> key <> ": " <> typeName <> "\n"
    in TIO.appendFile toFile writeStr

getIndent :: Int -> T.Text
getIndent x = T.pack $ getIndent' x ""

getIndent' :: Int -> String -> String
getIndent' 0 acc = acc
getIndent' x acc = getIndent' (x - 1) ("    " ++ acc)

-- instance Show PythonObj where
--     show PythonIntType = "int"
--     show PythonFloatType = "float"
--     show (Dataclass name map) = "dataclass"
--     show (PyLit x) = show x
--     show (PyType x) = show x


-- instance Converts (RecordField s a) where
--     convert rf = convert $ recordFieldValue rf

class Converts a where
    convert :: ConvertState -> a -> ConvertState

instance Converts (Expr s Import) where
    convert (ConvertState False objs) e = let
        cs' = ConvertState True objs
        in convert cs' $ denote e
    convert cs (Var _) = trace "Traced Var" cs
    convert cs (Note s expr) = trace "traced Note" $ convert cs expr
    convert cs (Lam _ _ _) = error "lam"
    convert cs (Pi _ _ _ _) = error "pi"
    convert cs (App _ _) = error "app"
    convert (ConvertState d objs) (Let (Binding _ name _ _ _ (Embed impt)) e) = let
        newPkg = getImportedPackage name impt
        cs' = ConvertState d (newPkg:objs)
        in convert cs' e
    convert (ConvertState d objs) (Let (Binding _ name _ _ _ (Record m)) e) = let
        newDc = getDataclass name m
        cs' = ConvertState d (newDc:objs)
        in convert cs' e
    convert cs (Let (Binding _ name _ _ _ (RecordLit m)) e) = let
        cs' = addPackage cs name m
        in convert cs' e
    convert (ConvertState d objs) (Let (Binding _ name _ _ _ attr) e) = let
        dcAttribute = getDataclassTypeAttribute name attr
        objs' = dcAttribute:objs
        cs' = ConvertState d objs'
        in convert cs' e
    -- convert (ConvertState d objs) (Let (Binding _ name _ _ _ (Double)) e) = let
    --         objs' = DataclassFloatAttribute (traceShowId name):objs
    --         cs' = ConvertState d objs'
    --         in convert cs' e
    -- convert (ConvertState objs) (Let (Binding _ name _ _ _ (Var v)) e) = error
    --     "it's a var"
    -- convert (ConvertState objs) (Let (Binding _ name _ _ _ (Record _)) e) = error
    --     "it's a record"
    -- convert (ConvertState objs) (Let (Binding _ name _ _ _ v) e) = let
    --         _ = traceShowId name
    --         _ = traceShowId $ typeOf v
    --         in error $ T.unpack name
    -- convert cs (Embed (Import (ImportHashed _ (Local Here (File dir file))) Code)) = let
    --     cs' = addPackage cs name m
    --     in convert cs' e
    -- convert cs (Embed imprt) =
    --     cs' = addSubPackage imprt
    --     error $ "Not implemented: dhall imports from URL"
    convert cs (Annot _ _) = error "annot"
    convert cs Bool = error "bool"
    convert cs (BoolLit _) = error "boollit"
    convert cs (BoolAnd _ _) = error "booland"
    convert cs (BoolOr _ _) = error "boolor"
    convert cs (BoolEQ _ _) = error "booleq"
    convert cs (BoolNE _ _) = error "boolne"
    convert cs (BoolIf _ _ _) = error "boolif"
    convert cs Natural = error "natural"
    convert cs (NaturalLit nat) = error "natural lit"
    convert cs NaturalFold = error "naturalfold"
    convert cs NaturalBuild = error "naturalbuild"
    convert cs NaturalIsZero = error "naturaliszero"
    convert cs NaturalEven = error "natural even"
    convert cs NaturalOdd = error "natural odd"
    convert cs NaturalToInteger = error "natural to integer"
    convert cs NaturalShow = error "natural show"
    convert cs NaturalSubtract = error "natural subtract"
    convert cs (NaturalPlus _ _) = error "natural plus"
    convert cs (NaturalTimes _ _) = error "natural times"
    convert cs Integer = error "integer"
    convert cs (IntegerLit _) = error "integer lit"
    convert cs IntegerClamp = error "integer clamp"
    convert cs IntegerNegate = error "integer negate"
    convert cs IntegerShow = error "integer show"
    convert cs IntegerToDouble = error "integer to double"
    convert cs Double = error "double" -- (cs, Just $ PythonFloatType)
    convert cs (DoubleLit _) = error "double lit"
    convert cs DoubleShow = error "double show"
    convert cs Text = error "text"
    convert cs (TextLit _) = error "text lit"
    convert cs (TextAppend _ _) = error "text append"
    convert cs TextReplace = error "text replace"
    convert cs TextShow = error "text show"
    convert cs Date = error "date"
    convert cs (DateLiteral _) = error "date literal"
    convert cs Time = error "time"
    convert cs (TimeLiteral _ _) = error "time literal"
    convert cs TimeZone = error "time zone"
    convert cs (TimeZoneLiteral _) = error "time zone literal"
    convert cs List = error "list"
    convert cs (ListLit _ _) = error "list lit"
    convert cs (ListAppend _ _) = error "list append"
    convert cs ListBuild = error "list build"
    convert cs ListFold = error "list fold"
    convert cs ListLength = error "list length"
    convert cs ListHead = error "list head"
    convert cs ListLast = error "list last"
    convert cs ListIndexed = error "list indexed"
    convert cs ListReverse = error "list reverse"
    convert cs Optional = error "optional"
    convert cs (Some _) = error "some"
    convert cs None = error "None"
    convert cs (Record _) = error "Record"
    convert cs (RecordLit _) = error "record lit"
    convert cs (Union _) = error "union"
    convert cs (Combine _ _ _ _) = error "combine"
    convert cs (CombineTypes _ _ _) = error "combine types"
    convert cs (Prefer _ _ _ _) = error "prefer"
    convert cs (RecordCompletion _ _) = error "record completion"
    convert cs (Merge _ _ _) = error "merge"
    convert cs (ToMap _ _) = error "to map"
    convert cs (ShowConstructor _) = error "show constructor"
    convert cs (Field _ _) = error "field"
    convert cs (Project _ _) = error "project"
    convert cs (Assert _) = error "assert"
    convert cs (Equivalent _ _ _) = error "equivalent"
    convert cs (With _ _ _) = error "with"
    convert cs (ImportAlt _ _) = error "import alt"
    convert cs (Embed _) = error "embed"

getImportedPackage :: T.Text -> Import -> PythonObj
getImportedPackage name (Import (ImportHashed hash (Local prefix file)) Code) = undefined
-- getImportedPackage name (Import (ImportHashed hash (Local prefix file)) Code) = let
--     path = show file
--     objE = dhallFileToPythonPackageObj path

getDataclass :: T.Text -> (Map T.Text (RecordField s a))-> PythonObj
getDataclass name map = let
    dcObjs = elems $ mapWithKey getMapField map
    in PythonDataclass name dcObjs

getMapField :: T.Text -> RecordField s a -> PythonObj
getMapField name (RecordField _ attr _ _) = getDataclassTypeAttribute name attr

getDataclassTypeAttribute :: T.Text -> Expr s a -> PythonObj
getDataclassTypeAttribute name Natural = DataclassIntAttribute name
getDataclassTypeAttribute name Double = DataclassFloatAttribute name
getDataclassTypeAttribute name Text = DataclassStrAttribute name
getDataclassTypeAttribute name (Var (V typeName _)) =
    DataclassUserDefinedTypeAttribute name typeName

addPackage :: ConvertState -> T.Text -> (Map T.Text (RecordField s a)) -> ConvertState
addPackage (ConvertState i objs) name map = let
    pkg = PythonPackage name $ reverse objs
    in ConvertState i [(trace ("Package is " ++ show pkg) pkg)]

data FileParseError = DhallError ParseError | PythonObjNotFound

dhallFileToPythonPackageObj :: FilePath -> IO (Either FileParseError PythonObj)
dhallFileToPythonPackageObj fromFile = do
    contents <- TIO.readFile fromFile
    let exprE = exprFromText fromFile contents
    let exprE' = denote <$> exprE
    let cs = newConvertState
    let objE = convert cs <$> exprE'
    case objE of
        Left err -> return $ Left $ DhallError err
        Right (ConvertState d objs) -> case objs of
            [pkg] -> return (Right pkg)
            _ -> return (Left PythonObjNotFound)

