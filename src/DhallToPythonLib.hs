module DhallToPythonLib where

import Data.Text
import Dhall.Core ( Expr(..), RecordField(..) )
import Dhall.Map as DhallMap
import Dhall.Parser ( Src )
import Data.Void ( Void )

data PythonType =
    PythonIntType
    | PythonFloatType
    | Dataclass (DhallMap.Map Text PythonObj)
    deriving Eq

instance Show PythonType where
    show PythonIntType = "int"
    show PythonFloatType = "float"
    show (Dataclass map) = "dataclass"

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
    convert Natural = PyType $ PythonIntType
    convert Double = PyType $ PythonFloatType
    convert (NaturalLit nat) = PyLit $ PythonInt (toInteger nat)
    convert (Record map) = toDataclass map

toDataclass :: DhallMap.Map Text (RecordField s a) -> PythonObj
toDataclass map = PyType $ Dataclass $ convert <$> map

