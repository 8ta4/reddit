module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^?), ix)
import Control.Monad (join)
import Data.Aeson.Types (FromJSONKey(..), FromJSONKeyFunction(..), Parser)
import Data.HashMap.Strict (HashMap, keys)
import Data.HashSet (fromList, HashSet, toList)
import Data.Hashable (Hashable(..))
import Data.List (isSuffixOf, isPrefixOf, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text, unpack, splitOn, pack)
import Data.Time.Clock (UTCTime)
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException, FromJSON(..), withText)
import GHC.Generics (Generic)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (httpLbs, parseRequest, setRequestHeaders)
import Network.URI (URI, parseURI, uriAuthority, uriPath, uriRegName, uriToString)
import Prelude
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (getFeedItems, getItemPublishDate, getItemLink, getItemContent, getItemTitle)
import Text.Feed.Types (Feed, Item)
import qualified Data.Text.IO as TIO

data CommentConfig = CommentConfig
  { text :: Text
  , threshold :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON CommentConfig

parseCommentURI :: Text -> Parser CommentURI
parseCommentURI t = case parseURI (unpack t) of
  Just uri -> case uriAuthority uri of
    Just auth -> if "reddit.com" `isSuffixOf` uriRegName auth && "/r/" `isPrefixOf` uriPath uri && (splitOn "/" (pack (uriPath uri)) ^? ix 3) == Just "comments"
      then pure (CommentURI uri)
      else fail "Invalid URI: not a Reddit comment"
    Nothing -> fail "Invalid URI: no authority"
  Nothing  -> fail "Invalid URI: could not parse URI"

newtype CommentURI = CommentURI URI
  deriving (Show, Eq, Generic)

instance FromJSON CommentURI where
  parseJSON = withText "URI" parseCommentURI

instance FromJSONKey CommentURI where
  fromJSONKey = FromJSONKeyTextParser parseCommentURI

instance Hashable CommentURI where
  hashWithSalt salt (CommentURI path) = hashWithSalt salt (uriToString id path "")

type Config = HashMap CommentURI CommentConfig

parseConfigFile :: FilePath -> IO (Either ParseException Config)
parseConfigFile = decodeFileEither

getSubredditURL :: CommentURI -> Text
getSubredditURL (CommentURI uri) = "https://www.reddit.com/r/" <> (splitOn "/" (pack $ uriPath uri) !! 2) <> "/.rss"

getSubredditURLs :: Config -> HashSet Text
getSubredditURLs = fromList . map getSubredditURL . keys

fetchRedditRSS :: Text -> IO (Maybe Feed)
fetchRedditRSS subredditURL = do
  request <- parseRequest $ unpack subredditURL
  -- TODO: add user agent
  let requestWithHeaders = setRequestHeaders [("User-Agent", "haskell:myApp:v1.0")] request
  response <- httpLbs requestWithHeaders
  let rssContent = responseBody response
  threadDelay 1000000 -- pause for 1 second
  return $ parseFeedSource rssContent

getPostData :: Item -> Maybe (Text, UTCTime)
getPostData p = do
  link <- getItemLink p
  pubDate <- join (getItemPublishDate p)
  return (link, pubDate)
  
printPostURL :: Text -> IO ()
printPostURL postURL = do
  rssFeed <- fetchRedditRSS $ postURL <> ".rss"
  case rssFeed of
    Nothing -> print $ "Error: could not fetch RSS feed for " <> postURL
    Just feed -> do
      case getFeedItems feed of
        [] -> print $ "Error: no items in RSS feed for " <> postURL
        (p:_) -> case getItemTitle p <> Just "\n" <> getItemContent p of
          Nothing -> print $ "Error: could not get title or content for " <> postURL
          Just _ -> TIO.putStrLn postURL
  
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
      rssFeeds <- catMaybes <$> mapM fetchRedditRSS (toList subredditURLs)
      let posts = concatMap getFeedItems rssFeeds

      let postData = mapMaybe getPostData posts
      let sortedData = sortOn (Down . snd) postData
      let postURLs = map fst sortedData
      
      print m
      print postURLs
      mapM_ printPostURL postURLs