module Main where

import Options.Applicative
import ReadWrite
import Data.Char ( isSpace )
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Dhall.Parser( exprFromText, ParseError )
import Dhall.Core( Expr, denote )

data CliOptions = CliOptions {
    showDhallExpr :: FilePath
} deriving Show

dhallExprParser = strOption (
    long "show-dhall-expr"
    <> help "Show Dhall source code expression for selected file"
    <> metavar "FILE"
    )

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions
                    <$> dhallExprParser
                    -- <*> verboseParser

removeSrc :: Expr s a -> Expr s a
removeSrc expr = denote expr

data ProcessState = ProcessState {
    processed::String
    , indentLevel::Int
}

getIndent :: Int -> String -> String
getIndent 0 acc = acc
getIndent x acc = getIndent (x - 1) ("    " ++ acc)

adjustIndent :: ProcessState -> String -> String
adjustIndent (ProcessState done ind) (c:rest)
    | Set.member c indSet = let
        newInd = ind + 1
        indent = getIndent newInd ""
        newDone = done ++ c:"\n" ++ indent
        in adjustIndent (ProcessState newDone newInd) rest
    | Set.member c outSet = let
        newInd = ind - 1
        indent = getIndent newInd ""
        newDone = done ++ "\n" ++ indent ++ [c] ++ "\n" ++ indent
        newRest = dropWhile isSpace rest
        in adjustIndent (ProcessState newDone newInd) newRest
    | c == ',' = let
        indent = getIndent ind ""
        newDone = done ++ "\n" ++ indent ++ [c]
        in adjustIndent (ProcessState newDone ind) rest
    | True = adjustIndent (ProcessState (done ++ [c]) ind) rest
    where
        indSet = Set.fromList ['(', '{']
        outSet = Set.fromList [')', '}']
adjustIndent (ProcessState done ind) [] = done

indent :: T.Text -> String
indent txt = let
    startingState = (ProcessState "" 0)
    in adjustIndent startingState (T.unpack txt)

-- addLineBreak :: T.Text -> Int -> T.Text
-- addLineBreak (',':cs) lvl = let
--     indent = T.concat (replicate lvl (T.Pack "    "))
--     in (T.Singleton '\n') <> indent <> cs
-- addLineBreak t _ = t

main :: IO ()
main = do
    opts <- execParser opts
    let fpath = showDhallExpr opts
    contents <- TIO.readFile fpath
    let parsed = exprFromText fpath contents
    case parsed of
        Left _ -> print "Could not parse file"
        Right src -> let
            srcText = T.pack . show . removeSrc $ src
            srcFmt = indent srcText
            in putStr srcFmt
  where
    opts = info (cliOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Utility for converting dhall values and types to Python"
     <> header "Dhall-to-python" )
