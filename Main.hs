module Main where

import HaskellSay (haskellSay)
import DhallToPythonLib

main :: IO ()
main = haskellSay $ "If inverted, False is " ++ writeBool (invert False)

