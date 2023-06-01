module Main (main) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Yaml (decodeFileEither, ParseException, prettyPrintParseException, FromJSON)
import GHC.Generics (Generic)
import Prelude
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

data UrlConfig = UrlConfig
  { text :: Text
  , threshold :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON UrlConfig

type Config = HashMap Text (HashMap Text UrlConfig)

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
    Right m -> print m
