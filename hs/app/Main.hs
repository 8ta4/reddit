module Main (main) where

import Data.Foldable (toList, traverse_)
import Data.List (sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down))
import Data.Yaml (prettyPrintParseException)
import Reddit
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Feed.Query (getFeedItems)
import Prelude

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let configFile = homeDir </> ".config" </> "reddit" </> "config.yaml"
  putStrLn configFile
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

      print m
      traverse_ (printPostURL m) postURLs
