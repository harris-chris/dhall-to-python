module Main where

import Options.Applicative
import ReadWrite

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
    print opts
  where
    opts = info (cliOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "Hello, this is a toy example for how to design command line interfaces in Haskell"
     <> header "Haskell-CLI-Example" )
