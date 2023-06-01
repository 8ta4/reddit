module Main (main) where

import Prelude
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  let configFile = homeDir </> ".config" </> "reddit" </> "config.yaml"
  putStrLn configFile