module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Lens (ix, (^?))
import Control.Monad (join)
import Data.Aeson.Types (FromJSONKey (..), FromJSONKeyFunction (..), Parser, ToJSON)
import Data.Foldable (toList, traverse_)
import Data.HashMap.Strict (HashMap, keys)
import Data.HashSet (HashSet, fromList)
import Data.Hashable (Hashable (..))
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down))
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime)
import Data.Yaml (FromJSON (..), ParseException, decodeFileEither, prettyPrintParseException, withText)
import GHC.Generics (Generic)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (getResponseBody, httpJSON, httpLbs, parseRequest, setRequestBodyJSON, setRequestHeaders, setRequestMethod, setRequestPath)
import Network.URI (URI, parseURI, uriAuthority, uriPath, uriRegName, uriToString)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Feed.Import (parseFeedSource)
import Text.Feed.Query (getFeedItems, getItemContent, getItemLink, getItemPublishDate, getItemTitle)
import Text.Feed.Types (Feed, Item)
import Prelude

data CommentConfig = CommentConfig
  { text :: Text,
    threshold :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON CommentConfig

parseCommentURI :: Text -> Parser CommentURI
parseCommentURI t = case parseURI (unpack t) of
  Just uri -> case uriAuthority uri of
    Just auth ->
      if "reddit.com" `isSuffixOf` uriRegName auth && "/r/" `isPrefixOf` uriPath uri && (splitOn "/" (pack (uriPath uri)) ^? ix 3) == Just "comments"
        then pure (CommentURI uri)
        else fail "Invalid URI: not a Reddit comment"
    Nothing -> fail "Invalid URI: no authority"
  Nothing -> fail "Invalid URI: could not parse URI"

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
  let requestWithHeaders = setRequestHeaders [("User-Agent", "hs:myApp:v1.0")] request
  response <- httpLbs requestWithHeaders
  let rssContent = responseBody response
  threadDelay 1000000 -- pause for 1 second
  return $ parseFeedSource rssContent

getPostData :: Item -> Maybe (Text, UTCTime)
getPostData p = do
  link <- getItemLink p
  pubDate <- join (getItemPublishDate p)
  return (link, pubDate)

data SimilarityRequest = SimilarityRequest
  { example :: Text,
    query :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SimilarityRequest

getSimilarityScore :: Text -> Text -> IO Double
getSimilarityScore concatenatedText text = do
  initialRequest <- parseRequest "http://localhost:8080"
  let request =
        setRequestMethod "POST"
          . setRequestPath "/"
          . setRequestBodyJSON (SimilarityRequest concatenatedText text)
          $ initialRequest
  response <- httpJSON request
  let score = getResponseBody response :: Double
  return score

printPostURL :: Config -> Text -> IO ()
printPostURL _ postURL = do
  rssFeed <- fetchRedditRSS $ postURL <> ".rss"
  case rssFeed of
    Nothing -> print $ "Error: could not fetch RSS feed for " <> postURL
    Just feed -> do
      case getFeedItems feed of
        [] -> print $ "Error: no items in RSS feed for " <> postURL
        (p : _) -> case getItemTitle p <> Just "\n" <> getItemContent p of
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
      rssFeeds <- catMaybes <$> traverse fetchRedditRSS (toList subredditURLs)
      let posts = concatMap getFeedItems rssFeeds

      let postData = mapMaybe getPostData posts
      let sortedData = sortOn (Down . snd) postData
      let postURLs = map fst sortedData

      print m
      traverse_ (printPostURL m) postURLs
