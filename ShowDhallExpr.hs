module Main where

import Options.Applicative
import ReadWrite
import qualified Data.Text.IO as TIO
import Dhall.Parser( exprFromText, ParseError )
import Dhall.Core( pretty )

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

main :: IO ()
main = do
    opts <- execParser opts
    let fpath = showDhallExpr opts
    contents <- TIO.readFile fpath
    let parsed = exprFromText fpath contents
    case parsed of
        Left _ -> print "Could not parse file"
        Right src -> print $ pretty src
  where
    opts = info (cliOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Utility for converting dhall values and types to Python"
     <> header "Dhall-to-python" )
