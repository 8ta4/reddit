module Main (main) where

import Data.Text (Text)
import Data.Yaml (prettyPrintParseException)
import Options.Applicative (Alternative ((<|>)), Parser, argument, command, execParser, fullDesc, header, helper, info, metavar, progDesc, str, subparser, (<**>))
import Reddit
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Prelude

data Command
  = Default
  | Scores Text

commandParser :: Parser Command
commandParser =
  pure Default
    <|> subparser
      ( command
          "scores"
          ( info
              (Scores <$> argument str (metavar "URL"))
              (progDesc "Calculate similarity scores for a specific post")
          )
      )

defaultAction :: IO ()
defaultAction = do
  homeDir <- getHomeDirectory
  let configFile = homeDir </> ".config" </> "reddit" </> "config.yaml"
  config <- parseConfigFile configFile
  case config of
    Left e -> putStrLn $ "Error: " ++ prettyPrintParseException e
    Right m -> fetchAndPrintPosts m

scoresAction :: Text -> IO ()
scoresAction url = do
  homeDir <- getHomeDirectory
  let configFile = homeDir </> ".config" </> "reddit" </> "config.yaml"
  config <- parseConfigFile configFile
  case config of
    Left e -> putStrLn $ "Error: " ++ prettyPrintParseException e
    Right m -> fetchAndPrintScores m url

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper) (fullDesc <> header "reddit - CLI for Reddit")
  case cmd of
    Default -> defaultAction
    Scores url -> scoresAction url
