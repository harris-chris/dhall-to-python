module ExprConversion where

import Debug.Trace ( trace, traceShowId )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable ( typeOf )
import Data.Void ( Void )
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), denote )
import Dhall.Map as DhallMap
import Dhall.Parser ( Src )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), (<.>), takeBaseName )
import Text.Casing ( quietSnake )

class Converts a where
    convert :: ConvertState -> a -> ConvertState

data ConvertState = ConvertState {
    isDenoted :: Bool
    , objs :: [PythonObj]
}

newConvertState = ConvertState False []

data PythonObj =
    PythonIntTypeSpec Text
    | PythonFloatTypeSpec Text
    | PythonDataclass T.Text [PythonObj]
    | PythonPackage T.Text [PythonObj]
    deriving (Eq, Show)

writePythonObj :: FilePath -> Int -> PythonObj -> IO ()
writePythonObj baseFolder indent (PythonPackage name objs) = let
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
        foldl (\io x -> io >> writePythonObj pyFpath indent x) (return ()) objs

writePythonObj toFile indent (PythonDataclass name objs) = let
    writeStr =
        "@dataclass(frozen=True, eq=True)\n" <>
        "class " <> name <> ":\n"
    in do
        TIO.appendFile toFile (trace ("writing " ++ T.unpack writeStr) writeStr)
        foldl (\io x -> io >> writePythonObj toFile (indent + 1) x) (return ()) objs

writePythonObj toFile indent (PythonIntTypeSpec name) = let
    writeStr = getIndent indent <> name <> ": int\n"
    in TIO.appendFile toFile writeStr

writePythonObj toFile indent (PythonFloatTypeSpec name) = let
    writeStr = getIndent indent <> name <> ": float\n"
    in TIO.appendFile toFile writeStr

getIndent :: Int -> Text
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

instance Converts (Expr s a) where
    convert (ConvertState False objs) e = let
        cs' = ConvertState True objs
        in convert cs' $ denote e
    convert cs (Var _) = trace "Traced Var" cs
    convert cs (Note s expr) = trace "traced Note" $ convert cs expr
    convert cs (Lam _ _ _) = error "lam"
    convert cs (Pi _ _ _ _) = error "pi"
    convert cs (App _ _) = error "app"
    convert (ConvertState d objs) (Let (Binding _ name _ _ _ (Natural)) e) = let
            objs' = PythonIntTypeSpec (traceShowId name):objs
            cs' = ConvertState d objs'
            in convert cs' e
    convert (ConvertState d objs) (Let (Binding _ name _ _ _ (Double)) e) = let
            objs' = PythonFloatTypeSpec (traceShowId name):objs
            cs' = ConvertState d objs'
            in convert cs' e
    -- convert (ConvertState objs) (Let (Binding _ name _ _ _ (Var v)) e) = error
    --     "it's a var"
    -- convert (ConvertState objs) (Let (Binding _ name _ _ _ (Record _)) e) = error
    --     "it's a record"
    -- convert (ConvertState objs) (Let (Binding _ name _ _ _ v) e) = let
    --         _ = traceShowId name
    --         _ = traceShowId $ typeOf v
    --         in error $ T.unpack name
    convert cs (Let (Binding _ name _ _ _ (Record m)) e) = let
        cs' = addDataclass cs (trace ("adding dataclass" ++ T.unpack name) name) m
        in convert cs' e
    convert cs (Let (Binding _ name _ _ _ (RecordLit m)) e) = let
        cs' = addPackage cs name m
        in convert cs' e
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

    -- convert cs e = let err = traceShowId e in error "Not matched"
    -- convert cs e = error "Not matched"
    -- convert cs Let (Binding _ var _ _ _ (Record map) = toDataclass map

-- toDataclass :: T.Text -> (DhallMap.Map T.Text (RecordField s a)) -> PythonObj
-- toDataclass name map = PyType $ Dataclass name $ convert <$> map
addDataclass :: ConvertState -> T.Text -> (Map T.Text (RecordField s a)) -> ConvertState
addDataclass (ConvertState i objs) name map = let
    dcObjs = elems $ mapWithKey getMapField map
    dc = PythonDataclass name dcObjs
    in ConvertState i ((trace ("dataclass is " ++ show dc) dc):objs)

getMapField :: T.Text -> RecordField s a -> PythonObj
getMapField name (RecordField _ Natural _ _) = PythonIntTypeSpec name
getMapField name (RecordField _ Double _ _) = PythonFloatTypeSpec name

addPackage :: ConvertState -> T.Text -> (Map T.Text (RecordField s a)) -> ConvertState
addPackage (ConvertState i objs) name map = let
    pkg = PythonPackage name objs
    in ConvertState i [(trace ("Package is " ++ show pkg) pkg)]

