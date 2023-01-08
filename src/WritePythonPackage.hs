module WritePythonPackage where

import Control.Monad (join)
import Data.Maybe ( fromJust, isJust )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace ( trace, traceShowId )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ( (</>), (<.>), takeBaseName )
import System.FilePath.Posix (takeDirectory)
import qualified Data.Text

import Dhall.Core( denote )

import PythonPackage

writePythonObj :: PythonOptions -> FilePath -> Int -> PythonObj -> IO ()
writePythonObj pyOpts baseFolder indent (PythonPackage name objs) = let
    pyFname = (T.unpack name) <.> "py"
    pyFpath =  baseFolder </> pyFname
    initFpath = baseFolder </> "__init__.py"
    initContents = T.pack $ "from ." <> (T.unpack name) <> " import " <> (T.unpack name)
    pyHeader = "from dataclasses import dataclass\n\n"
    in do
        createDirectoryIfMissing False baseFolder
        TIO.writeFile initFpath initContents
        TIO.writeFile pyFpath pyHeader
        foldl (\io x -> io
            >> writePythonObj pyOpts pyFpath indent x
            >> appendFile pyFpath "\n"
            ) (return ()) objs

writePythonObj pyOpts toFile indent (PythonDataclass name objs) = let
    dataclassParams = getDataclassParameters $ dataclassOptions pyOpts
    writeStr =
        "@dataclass" <> dataclassParams <> "\n" <>
        "class " <> name <> ":\n"
    in do
        TIO.appendFile toFile (trace ("writing " ++ T.unpack writeStr) writeStr)
        foldl
            (\io x -> io >> writePythonObj pyOpts toFile (indent + 1) x)
            (return ())
            objs

writePythonObj pyOpts toFile indent (DataclassFloatAttribute name) = let
    writeStr = getIndent indent <> name <> ": float\n"
    in TIO.appendFile toFile writeStr

writePythonObj pyOpts toFile indent (DataclassIntAttribute name) = let
    writeStr = getIndent indent <> name <> ": int\n"
    in TIO.appendFile toFile writeStr

writePythonObj pyOpts toFile indent (DataclassStrAttribute name) = let
    writeStr = getIndent indent <> name <> ": str\n"
    in TIO.appendFile toFile writeStr

writePythonObj pyOpts toFile indent (DataclassUserDefinedTypeAttribute key typeName) = let
    writeStr = getIndent indent <> key <> ": " <> typeName <> "\n"
    in TIO.appendFile toFile writeStr

getIndent :: Int -> T.Text
getIndent x = T.pack $ getIndent' x ""

getIndent' :: Int -> String -> String
getIndent' 0 acc = acc
getIndent' x acc = getIndent' (x - 1) ("    " ++ acc)

