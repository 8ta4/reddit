module Main (main) where

import Data.Foldable (toList, traverse_)
import Data.List (sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down))
import Data.Yaml (prettyPrintParseException)
import Options.Applicative (Alternative ((<|>)), Parser, argument, command, execParser, fullDesc, header, helper, info, metavar, progDesc, str, subparser, (<**>))
import Reddit
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Feed.Query (getFeedItems)
import Prelude

data Command
  = Default
  | Scores String

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
    Right m -> do
      let subredditURLs = getSubredditURLs m
      rssFeeds <- catMaybes <$> traverse fetchRedditRSS (toList subredditURLs)
      let posts = concatMap getFeedItems rssFeeds
      let postData = mapMaybe getPostData posts
      let sortedData = sortOn (Down . snd) postData
      let postURLs = map fst sortedData
      traverse_ (printPostURL m) postURLs

scoresAction :: String -> IO ()
scoresAction url = do
  putStrLn $ "Calculating scores for: " ++ url

-- Implement your logic for calculating scores here

main :: IO ()
main = do
  cmd <- execParser $ info (commandParser <**> helper) (fullDesc <> header "reddit - CLI for Reddit")
  case cmd of
    Default -> defaultAction
    Scores url -> scoresAction url
