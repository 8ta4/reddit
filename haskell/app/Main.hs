module Main (main) where

import Control.Lens ((^?), ix)
import Data.Aeson.Types (FromJSONKey(..), FromJSONKeyFunction(..), Parser)
import Data.HashMap.Strict (HashMap, keys)
import Data.HashSet (fromList, HashSet)
import Data.Hashable (Hashable(..))
import Data.List (isSuffixOf, isPrefixOf)
import Data.Text (Text, unpack, splitOn, pack)
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException, FromJSON(..), withText)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI, uriAuthority, uriPath, uriRegName, uriToString)
import Prelude
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

data CommentConfig = CommentConfig
  { text :: Text
  , threshold :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON CommentConfig

parseCommentURI :: Text -> Parser CommentURI
parseCommentURI t = case parseURI (unpack t) of
  Just uri -> case uriAuthority uri of
    Just auth -> if "reddit.com" `isSuffixOf` uriRegName auth && "/r/" `isPrefixOf` uriPath uri && splitOn "/" (pack (uriPath uri)) ^? ix 3 == Just "comments"
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
  
type Topic = HashMap CommentURI CommentConfig

type Config = HashMap Text Topic

parseConfigFile :: FilePath -> IO (Either ParseException Config)
parseConfigFile = decodeFileEither

getSubredditName :: CommentURI -> Text
getSubredditName (CommentURI uri) = splitOn "/" (pack $ uriPath uri) !! 2

getSubreddits :: Topic -> HashSet Text
getSubreddits = fromList . map getSubredditName . keys

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let configFile = homeDir </> ".config" </> "reddit" </> "config.yaml"
  putStrLn configFile
  config <- parseConfigFile configFile
  case config of
    Left e -> putStrLn $ "Error: " ++ prettyPrintParseException e
    Right m -> print m