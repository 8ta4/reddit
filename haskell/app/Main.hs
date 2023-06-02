module Main (main) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text, unpack)
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException, FromJSON(..), withText)
import Data.Aeson.Types (FromJSONKey(..), FromJSONKeyFunction(..), Parser)
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import Network.URI (URI, parseURI, uriAuthority, uriRegName, uriToString)
import Prelude
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Data.List (isSuffixOf)

data UrlConfig = UrlConfig
  { text :: Text
  , threshold :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON UrlConfig

parseComment :: Text -> Parser Comment
parseComment t = case parseURI (unpack t) of
  Just uri -> case uriAuthority uri of
    Just auth -> if "reddit.com" `isSuffixOf` uriRegName auth
      then pure (Comment uri)
      else fail "Invalid URI"
    Nothing -> fail "Invalid URI"
  Nothing  -> fail "Invalid URI"

newtype Comment = Comment URI
  deriving (Show, Eq, Generic)

instance FromJSON Comment where
  parseJSON = withText "URI" parseComment
        
instance FromJSONKey Comment where
  fromJSONKey = FromJSONKeyTextParser parseComment

instance Hashable Comment where
  hashWithSalt salt (Comment path) = hashWithSalt salt (uriToString id path "")

type Config = HashMap Text (HashMap Comment UrlConfig)

parseConfigFile :: FilePath -> IO (Either ParseException Config)
parseConfigFile = decodeFileEither

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let configFile = homeDir </> ".config" </> "reddit" </> "config.yaml"
  putStrLn configFile
  result <- parseConfigFile configFile
  case result of
    Left e -> putStrLn $ "Error: " ++ prettyPrintParseException e
    Right m -> do
      print (m :: Config)