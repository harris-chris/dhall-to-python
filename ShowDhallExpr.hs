module Main where

import Options.Applicative
import DhallExprUtils
import ReadWrite
import Data.Char ( isSpace )
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Dhall.Parser( exprFromText, ParseError )
import Dhall.Core( Expr, denote )

data CliOptions = CliOptions {
    showDhallExpr :: FilePath
    , denoteExpr :: Bool
} deriving Show

dhallExprParser = strOption (
    long "show-dhall-expr"
    <> help "Show Dhall source code expression for selected file"
    <> metavar "FILE"
    )
denoteParser = switch (
    long "denote"
    <> short 'd'
    <> help "Whether to remove Note expressions"
    )

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions
                    <$> dhallExprParser
                    <*> denoteParser

main :: IO ()
main = do
    opts <- execParser opts
    let fpath = showDhallExpr opts
    let shouldDenote = denoteExpr opts
    contents <- TIO.readFile fpath
    let parsed = exprFromText fpath contents
    case parsed of
        Left _ -> print "Could not parse file"
        Right expr -> let
            showOptions = ShowOptions shouldDenote Nothing
            exprTxt = showExpr showOptions expr
            -- exprFmt = indent exprText
            in TIO.putStr exprTxt
    where
        opts = info (cliOptionsParser <**> helper)
          ( fullDesc
         <> progDesc "Utility for converting dhall values and types to Python"
         <> header "Dhall-to-python" )
