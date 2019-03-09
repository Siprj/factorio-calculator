module Main where

import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

import Lib


basicOptions :: FilePath -> Parser Configuration
basicOptions home = Configuration
    <$> strOption
        ( long "raw-data"
        <> short 'r'
        <> metavar "DIR"
        <> help "Raw data exported from factorio-calculator mod"
        <> value (home </> ".factorio/script-output/data-raw.json")
        )
    <*> strOption
        ( long "mods-dir"
        <> short 'm'
        <> metavar "DIR"
        <> help "Directory with factorio mods"
        <> value (home </> ".factorio/mods")
        )
    <*> strOption
        ( long "data-dir"
        <> short 'd'
        <> metavar "DIR"
        <> help "Factorio data dir"
        <> value (home </> ".steam/steam/steamapps/common/Factorio/data")
        )
    <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "DIR"
        <> help "How enthusiastically to greet"
        )

opts :: FilePath -> ParserInfo Configuration
opts home = info (basicOptions home <**> helper) fullDesc

main :: IO ()
main = do
    home <- getHomeDirectory
    execParser (opts home) >>= run
