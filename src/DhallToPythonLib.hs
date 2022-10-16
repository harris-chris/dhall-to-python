module DhallToPythonLib where

import Data.Text
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

data ParsedPackage = ParsedPackage {
    binding_name :: Text
    , objs :: [PythonObj]
    , packages :: [ParsedPackage]
} deriving (Show, Eq)

class Converts a where
    convert :: a -> PythonObj

instance Converts (RecordField s a) where
    convert rf = convert $ recordFieldValue rf

instance Converts (Expr s a) where
    convert Natural = PyType $ PythonIntType
    convert Double = PyType $ PythonFloatType
    convert (NaturalLit nat) = PyLit $ PythonInt (toInteger nat)
    convert e = error $ show e
    -- convert Let (Binding _ var _ _ _ (Record map) = toDataclass map

toDataclass :: Text -> (DhallMap.Map Text (RecordField s a)) -> PythonObj
toDataclass name map = PyType $ Dataclass name $ convert <$> map

