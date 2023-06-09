module Reddit
  ( Config,
    parseConfigFile,
    fetchAndPrintPosts,
    fetchAndPrintScores,
    getSimilarityScore,
  )
where

import Control.Concurrent (threadDelay)
import Control.Lens (ix, (^?))
import Control.Monad (join, when)
import Data.Aeson.Types (FromJSONKey (..), FromJSONKeyFunction (..), Parser, ToJSON)
import Data.Foldable (traverse_)
import Data.Foldable qualified as F
import Data.HashMap.Strict (HashMap, elems, keys)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet, fromList)
import Data.Hashable (Hashable (..))
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime)
import Data.Yaml (FromJSON (..), ParseException, decodeFileEither, withText)
import GHC.Generics (Generic)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple (getResponseBody, httpJSON, httpLbs, parseRequest, setRequestBodyJSON, setRequestHeaders, setRequestMethod, setRequestPath)
import Network.URI (URI, parseURI, uriAuthority, uriPath, uriRegName, uriToString)
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
  let requestWithHeaders = setRequestHeaders [("User-Agent", "reddit")] request
  response <- httpLbs requestWithHeaders
  let rssContent = responseBody response
  threadDelay 1000000 -- pause for 1 second
  pure $ parseFeedSource rssContent

getPostData :: Item -> Maybe (Text, UTCTime)
getPostData p = do
  link <- getItemLink p
  pubDate <- join (getItemPublishDate p)
  pure (link, pubDate)

data SimilarityRequest = SimilarityRequest
  { example :: Text,
    query :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SimilarityRequest

getSimilarityScore :: Text -> Text -> IO Double
getSimilarityScore text1 text2 = do
  initialRequest <- parseRequest "http://localhost:8080"
  let request =
        setRequestMethod "POST"
          . setRequestPath "/"
          . setRequestBodyJSON (SimilarityRequest text1 text2)
          $ initialRequest
  response <- httpJSON request
  let score = getResponseBody response :: Double
  pure score

checkSimilarityScores :: Config -> Text -> IO Bool
checkSimilarityScores config postText = do
  let checkScore commentConfig = do
        similarityScore <- getSimilarityScore postText (text commentConfig)
        pure $ similarityScore >= threshold commentConfig

  results <- mapM checkScore (elems config)
  pure $ or results

getPostText :: Text -> IO (Maybe Text)
getPostText postURL = do
  rssFeed <- fetchRedditRSS $ postURL <> ".rss"
  case rssFeed of
    Nothing -> do
      print $ "Error: could not fetch RSS feed for " <> postURL
      pure Nothing
    Just feed -> do
      case getFeedItems feed of
        [] -> do
          print $ "Error: no items in RSS feed for " <> postURL
          pure Nothing
        (p : _) -> pure $ getItemTitle p <> Just "\n" <> getItemContent p

printPostURL :: Config -> Text -> IO ()
printPostURL config postURL = do
  postText <- getPostText postURL
  case postText of
    Nothing -> pure ()
    Just postText' -> do
      shouldPrint <- checkSimilarityScores config postText'
      when shouldPrint $ TIO.putStrLn postURL

fetchAndPrintPosts :: Config -> IO ()
fetchAndPrintPosts m = do
  let subredditURLs = getSubredditURLs m
  rssFeeds <- catMaybes <$> traverse fetchRedditRSS (F.toList subredditURLs)
  let posts = concatMap getFeedItems rssFeeds
  let postData = mapMaybe getPostData posts
  let sortedData = sortOn (Down . snd) postData
  let postURLs = map fst sortedData
  traverse_ (printPostURL m) postURLs

uriFromCommentURI :: CommentURI -> String
uriFromCommentURI (CommentURI uri) = show uri

printScore :: (CommentURI, Double) -> IO ()
printScore (commentURI, score) = putStrLn $ uriFromCommentURI commentURI ++ " " ++ show score

fetchAndPrintScores :: Config -> Text -> IO ()
fetchAndPrintScores config postURL = do
  postText <- getPostText postURL
  case postText of
    Nothing -> pure ()
    Just postText' -> do
      let calcScore (commentURI, commentConfig) = do
            similarityScore <- getSimilarityScore postText' (text commentConfig)
            pure (commentURI, similarityScore)

      scores <- mapM calcScore (HM.toList config)
      traverse_ printScore scores
