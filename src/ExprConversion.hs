module ExprConversion where

import Debug.Trace ( traceShowId )
import Data.Text
import Data.Typeable ( typeOf )
import Data.Void ( Void )
import Dhall.Core ( Binding(..), Expr(..), RecordField(..), denote )
import Dhall.Map as DhallMap
import Dhall.Parser ( Src )
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
    | PythonDataclass Text [PythonObj]
    | PythonPackage Text [PythonObj]
    deriving Eq

writePythonObj :: PythonObj -> FilePath -> IO ()
writePythonObj (PythonPackage name objs) baseFolder = let
    pyFname = snake name <.> "py"
    pyFpath = toFolder </> pyFname
    pyContents = T.pack $ foldl (\acc x -> acc ++ "\n\n" ++ (show x)) "" $ objs output
    init_fpath = toFolder </> "__init__.py"
    init_contents = "from ." <> (T.pack from_file) <> " import " <> (binding_name output)
    in do
        createDirectoryIfMissing False to_folder
        TIO.writeFile py_fpath pyContents
        TIO.writeFile init_fpath init_contents

writePythonObj (PythonDataclass name objs) to_file =

toCamelCase

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
    convert cs (Let _ _) = error "let"
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

-- toDataclass :: Text -> (DhallMap.Map Text (RecordField s a)) -> PythonObj
-- toDataclass name map = PyType $ Dataclass name $ convert <$> map
toDataclass :: ConvertState -> Text -> (Map Text (RecordField s a)) -> (ConvertState, Maybe PythonObj)
toDataclass cs txt map = undefined

toPackage :: ConvertState -> Text -> (Map Text (RecordField s a)) -> (ConvertState, Maybe PythonObj)
toPackage cs txt map = undefined

