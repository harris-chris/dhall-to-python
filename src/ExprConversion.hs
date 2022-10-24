module ExprConversion where

import Debug.Trace ( traceShowId )
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
import Text.Casing ( snake )

dhallExprToPythonObj :: Expr s a -> Maybe PythonObj
dhallExprToPythonObj expr = let
    (cs, obj) = (convert (ConvertState [])) . denote $ expr
    in obj

class Converts a where
    convert :: ConvertState -> a -> (ConvertState, Maybe PythonObj)

data ConvertState = ConvertState {
    objs :: [PythonObj]
}

data PythonObj =
    PythonIntType
    | PythonFloatType
    | PythonDataclass T.Text [PythonObj]
    | PythonPackage T.Text [PythonObj]
    deriving Eq

writePythonObj :: FilePath -> Int -> PythonObj -> IO ()
writePythonObj baseFolder indent (PythonPackage name objs) = let
    pyFname = snake (T.unpack name) <.> "py"
    pyFpath = baseFolder </> pyFname
    initFpath = baseFolder </> "__init__.py"
    initContents = "from ." <> name <> " import " <> name
    in do
        createDirectoryIfMissing False baseFolder
        TIO.writeFile initFpath initContents
        foldl (\acc x -> acc >> writePythonObj pyFpath indent x) (return ()) objs

writePythonObj toFile indent (PythonDataclass name objs) = let
    writeStr =
        "@dataclass(frozen=True, eq=True)\n" <>
        "class " <> name <> ":\n"
    in do
        TIO.writeFile toFile writeStr
        foldl (\acc x -> acc >> writePythonObj toFile (indent + 1) x) (return ()) objs

-- instance Show PythonObj where
--     show PythonIntType = "int"
--     show PythonFloatType = "float"
--     show (Dataclass name map) = "dataclass"
--     show (PyLit x) = show x
--     show (PyType x) = show x


-- instance Converts (RecordField s a) where
--     convert rf = convert $ recordFieldValue rf

instance Converts (Expr s a) where
    convert cs (Const _) = error "const"
    convert cs (Var _) = error "var"
    convert cs (Lam _ _ _) = error "lam"
    convert cs (Pi _ _ _ _) = error "pi"
    convert cs (App _ _) = error "app"
    convert cs (Let binding expr) = case expr of
        (Let (Binding _ name _ _ _ _) (RecordLit map)) ->
            toPackage cs name map
        (Let (Binding _ name _ _ _ _) (Record map)) ->
            toDataclass cs name map
        _ -> (cs, Nothing)
    convert cs (Annot _ _) = error "annot"
    convert cs Bool = error "bool"
    convert cs (BoolLit _) = error "boollit"
    convert cs (BoolAnd _ _) = error "booland"
    convert cs (BoolOr _ _) = error "boolor"
    convert cs (BoolEQ _ _) = error "booleq"
    convert cs (BoolNE _ _) = error "boolne"
    convert cs (BoolIf _ _ _) = error "boolif"
    convert cs Natural = (cs, Just $ PythonIntType)
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
    convert cs Double = (cs, Just $ PythonFloatType)
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
    -- convert cs (Record map) = toDataclass map
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
    convert cs (Note s expr) = convert cs expr
    convert cs (ImportAlt _ _) = error "import alt"
    convert cs (Embed _) = error "embed"

    -- convert cs e = let err = traceShowId e in error "Not matched"
    -- convert cs e = error "Not matched"
    -- convert cs Let (Binding _ var _ _ _ (Record map) = toDataclass map

-- toDataclass :: T.Text -> (DhallMap.Map T.Text (RecordField s a)) -> PythonObj
-- toDataclass name map = PyType $ Dataclass name $ convert <$> map
toDataclass :: ConvertState -> T.Text -> (Map T.Text (RecordField s a)) -> (ConvertState, Maybe PythonObj)
toDataclass cs txt map = undefined

toPackage :: ConvertState -> T.Text -> (Map T.Text (RecordField s a)) -> (ConvertState, Maybe PythonObj)
toPackage cs txt map = undefined

