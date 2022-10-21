module ExprConversion where

import Debug.Trace ( traceShowId )

import Data.Text
import Data.Typeable ( typeOf )
import Dhall.Core ( Expr(..), RecordField(..) )
import Dhall.Map as DhallMap
import Dhall.Parser ( Src )
import Data.Void ( Void )

data PythonType =
    PythonIntType
    | PythonFloatType
    | Dataclass Text (DhallMap.Map Text PythonObj)
    deriving Eq

instance Show PythonType where
    show PythonIntType = "int"
    show PythonFloatType = "float"
    show (Dataclass name map) = "dataclass"

data PythonLit =
    PythonInt Integer
    deriving Eq

instance Show PythonLit where
    show (PythonInt i) = show i

data PythonObj =
    PyLit PythonLit
    | PyType PythonType
    deriving Eq

instance Show PythonObj where
    show (PyLit x) = show x
    show (PyType x) = show x


class Converts a where
    convert :: a -> PythonObj

instance Converts (RecordField s a) where
    convert rf = convert $ recordFieldValue rf

instance Converts (Expr s a) where
    convert (Const _) = error "const"
    convert (Var _) = error "var"
    convert (Lam _ _ _) = error "lam"
    convert (Pi _ _ _ _) = error "pi"
    convert (App _ _) = error "app"
    convert (Let _ _) = error "let"
    convert (Annot _ _) = error "annot"
    convert Bool = error "bool"
    convert (BoolLit _) = error "boollit"
    convert (BoolAnd _ _) = error "booland"
    convert (BoolOr _ _) = error "boolor"
    convert (BoolEQ _ _) = error "booleq"
    convert (BoolNE _ _) = error "boolne"
    convert (BoolIf _ _ _) = error "boolif"
    convert Natural = PyType $ PythonIntType
    convert (NaturalLit nat) = PyLit $ PythonInt (toInteger nat)
    convert NaturalFold = error "naturalfold"
    convert NaturalBuild = error "naturalbuild"
    convert NaturalIsZero = error "naturaliszero"
    convert NaturalEven = error "natural even"
    convert NaturalOdd = error "natural odd"
    convert NaturalToInteger = error "natural to integer"
    convert NaturalShow = error "natural show"
    convert NaturalSubtract = error "natural subtract"
    convert (NaturalPlus _ _) = error "natural plus"
    convert (NaturalTimes _ _) = error "natural times"
    convert Integer = error "integer"
    convert (IntegerLit _) = error "integer lit"
    convert IntegerClamp = error "integer clamp"
    convert IntegerNegate = error "integer negate"
    convert IntegerShow = error "integer show"
    convert IntegerToDouble = error "integer to double"
    convert Double = PyType $ PythonFloatType
    convert (DoubleLit _) = error "double lit"
    convert DoubleShow = error "double show"
    convert Text = error "text"
    convert (TextLit _) = error "text lit"
    convert (TextAppend _ _) = error "text append"
    convert TextReplace = error "text replace"
    convert TextShow = error "text show"
    convert Date = error "date"
    convert (DateLiteral _) = error "date literal"
    convert Time = error "time"
    convert (TimeLiteral _ _) = error "time literal"
    convert TimeZone = error "time zone"
    convert (TimeZoneLiteral _) = error "time zone literal"
    convert List = error "list"
    convert (ListLit _ _) = error "list lit"
    convert (ListAppend _ _) = error "list append"
    convert ListBuild = error "list build"
    convert ListFold = error "list fold"
    convert ListLength = error "list length"
    convert ListHead = error "list head"
    convert ListLast = error "list last"
    convert ListIndexed = error "list indexed"
    convert ListReverse = error "list reverse"
    convert Optional = error "optional"
    convert (Some _) = error "some"
    convert None = error "None"
    -- convert (Record map) = toDataclass map
    convert (RecordLit _) = error "record lit"
    convert (Union _) = error "union"
    convert (Combine _ _ _ _) = error "combine"
    convert (CombineTypes _ _ _) = error "combine types"
    convert (Prefer _ _ _ _) = error "prefer"
    convert (RecordCompletion _ _) = error "record completion"
    convert (Merge _ _ _) = error "merge"
    convert (ToMap _ _) = error "to map"
    convert (ShowConstructor _) = error "show constructor"
    convert (Field _ _) = error "field"
    convert (Project _ _) = error "project"
    convert (Assert _) = error "assert"
    convert (Equivalent _ _ _) = error "equivalent"
    convert (With _ _ _) = error "with"
    convert (Note s expr) = convert expr
    convert (ImportAlt _ _) = error "import alt"
    convert (Embed _) = error "embed"

    -- convert e = let err = traceShowId e in error "Not matched"
    -- convert e = error "Not matched"
    -- convert Let (Binding _ var _ _ _ (Record map) = toDataclass map

toDataclass :: Text -> (DhallMap.Map Text (RecordField s a)) -> PythonObj
toDataclass name map = PyType $ Dataclass name $ convert <$> map

