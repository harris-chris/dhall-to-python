module DhallToPythonLib where

-- import Data.Map (Map)
import Data.Text
import Dhall.Core ( Expr(..), RecordField(..) )
import Dhall.Map ( Map(..) )
import Dhall.Parser ( Src )
import Dhall.Src
import Data.Void ( Void )

data PythonType =
    PythonIntType
    | OptionalType PythonType
    | Dataclass (Map Text PythonObj)

data PythonLit =
    PythonInt Integer

data PythonObj = Lit PythonLit | Type PythonType

class Converts a where
    convert :: a -> PythonObj

instance Converts (RecordField s a) where
    convert rf = convert $ recordFieldValue rf

instance Converts (Expr s a) where
    convert (NaturalLit nat) = Lit $ PythonInt (toInteger nat)
    convert (NaturalLit nat) = Lit $ PythonInt (toInteger nat)
    convert (Record map) = toDataclass map

toDataclass :: Map Text (RecordField s a) -> PythonObj
toDataclass map = Type $ Dataclass $ convert <$> map

