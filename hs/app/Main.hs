module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Yaml (prettyPrintParseException)
import Network.HTTP.Client (HttpException (..))
import Options.Applicative (Alternative ((<|>)), Parser, argument, command, execParser, fullDesc, header, helper, info, metavar, progDesc, str, subparser, (<**>))
import Reddit (Config, fetchAndPrintPosts, fetchAndPrintScores, getSimilarityScore, parseConfigFile)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Process (CreateProcess (cwd), createProcess, proc)
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

withConfig :: (Config -> IO ()) -> IO ()
withConfig action = do
  homeDir <- getHomeDirectory
  let configFile = homeDir </> ".config" </> "reddit" </> "config.yaml"
  config <- parseConfigFile configFile
  case config of
    Left e -> putStrLn $ "Error: " ++ prettyPrintParseException e
    Right m -> action m

defaultAction :: IO ()
defaultAction = withConfig fetchAndPrintPosts

scoresAction :: Text -> IO ()
scoresAction url = withConfig (`fetchAndPrintScores` url)

isServerRunning :: IO Bool
isServerRunning = do
  (getSimilarityScore "test" "test" >> pure True) `catch` handleHttpException
  where
    handleHttpException :: HttpException -> IO Bool
    handleHttpException _ = pure False

startClojureServer :: IO ()
startClojureServer = do
  _ <- createProcess (proc "lein" ["run"]) {cwd = Just "../clj"}
  pure ()

waitForServer :: IO ()
waitForServer = do
  isRunning <- isServerRunning
  if isRunning
    then pure ()
    else do
      threadDelay 1000000
      waitForServer

main :: IO ()
main = do
  isRunning <- isServerRunning
  unless isRunning startClojureServer
  waitForServer
  cmd <- execParser $ info (commandParser <**> helper) (fullDesc <> header "reddit - CLI for Reddit")
  case cmd of
    Default -> defaultAction
    Scores url -> scoresAction url
