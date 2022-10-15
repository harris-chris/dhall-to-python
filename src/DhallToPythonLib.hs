module DhallToPythonLib where

import Data.Map (Map)
import qualified Data.Text as T
import Dhall.Core ( Expr(..) )
import Dhall.Parser ( Src )
import Data.Void ( Void )

data PythonType =
    PythonIntType
    | OptionalType PythonType
    | Dataclass (Map T.Text PythonType)

data PythonLit =
    PythonInt Integer

data PythonObj = Lit PythonLit | Type PythonType

class Converts a where
    convert :: a -> PythonObj

instance Converts (Expr s a) where
    convert (NaturalLit nat) = Lit $ (PythonInt (toInteger nat))

