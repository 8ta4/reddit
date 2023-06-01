module Main (main) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text, unpack)
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException, FromJSON(..), withText)
import Data.Aeson.Types (FromJSONKey(..), FromJSONKeyFunction(..), Parser)
import Data.Hashable (Hashable(..))
import GHC.Generics (Generic)
import Network.URI (URI, parseURI, uriToString)
import Prelude
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

data UrlConfig = UrlConfig
  { text :: Text
  , threshold :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON UrlConfig

instance FromJSON URI where
  parseJSON = withText "URI" $ \t ->
    case parseURI (unpack t) of
      Just uri -> pure uri
      Nothing  -> fail "Invalid URI"
      
parseUri :: Text -> Parser URI
parseUri t = case parseURI (unpack t) of
  Just uri -> pure uri
  Nothing  -> fail "Invalid URI"

instance FromJSONKey URI where
  fromJSONKey = FromJSONKeyTextParser parseUri

instance Hashable URI where
  hashWithSalt salt uri = hashWithSalt salt (uriToString id uri "")

type Config = HashMap Text (HashMap URI UrlConfig)

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