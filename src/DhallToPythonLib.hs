module DhallToPythonLib where
import Dhall.Parser
import qualified Data.Text as T

invert :: Bool -> Bool
invert True = False
invert False = True

writeBool :: Bool -> String
writeBool True = "True"
writeBool False = "False"
