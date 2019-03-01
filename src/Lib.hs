{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( ZipPath
    , PahtPrefix
    , unzipImage
    , run
    , Recipe(..)
    ) where

import Control.Applicative ((<|>))
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import Data.Functor
import Data.Aeson hiding (Result)
import Codec.Archive.Zip
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeDirectory)
import System.Directory
import Data.Map
import qualified Data.Map as M
import qualified Data.List as L

import RawData

type ZipPath = FilePath
type PahtPrefix = FilePath


unzipImage :: ZipPath -> FilePath -> PahtPrefix -> String -> IO ()
unzipImage zipPath file pathPrefix version = do
    createDirectoryIfMissing True $ takeDirectory filePath
    mkEntrySelector fileVersionedPath
        >>= withArchive zipPath . getEntry
        >>= B.writeFile filePath
  where
    fileVersionedPath = pathPrefix <> "_" <> version </> file
    filePath = pathPrefix </> file

data RecipeWithImage = RecipeWithImage
    { name :: String
    , icon :: Map String IconPart
    }
  deriving (Show)

pairRecipeAndImages :: RawData -> [RecipeWithImage]
pairRecipeAndImages rawData@RawData{..} =
    fmap (pairRecipeAndImage rawData) $ elems recipe

pairRecipeAndImage
    :: RawData
    -> Recipe
    -> RecipeWithImage
pairRecipeAndImage RawData{..} Recipe{..} = RecipeWithImage
    { name = recipeName
    , icon = fromMaybe M.empty $
        fmap toSingleIcon recipeIcon
        <|> recipeIcons
        <|> resultIcon
        <|> resultsIcon
        <|> normalResultIcon
        <|> normalResultsIcon
    }
  where
    resultIcon :: Maybe (Map String IconPart)
    resultIcon = recipeResult >>= getItemFluidIcon

    resultsIcon :: Maybe (Map String IconPart)
    resultsIcon = recipeResults
        >>= (listToMaybe . elems)
        >>= (\Results{..} -> getItemFluidIcon resultName)

    normalResultIcon :: Maybe (Map String IconPart)
    normalResultIcon = recipeNormal
        >>= inputOutputResult >>= getItemFluidIcon

    normalResultsIcon :: Maybe (Map String IconPart)
    normalResultsIcon = recipeNormal
        >>= inputOutputResults
        >>= (listToMaybe . elems)
        >>= (\Ingredient{..} -> getItemFluidIcon ingredientName)

    toSingleIcon :: String -> Map String IconPart
    toSingleIcon = M.singleton "1" . toIconPart

    toIconPart v = IconPart v Nothing Nothing

    getItemFluidIcon :: String -> Maybe (Map String IconPart)
    getItemFluidIcon name = magic item itemIcon itemIcons
        <|> magic fluid fluidIcon fluidIcons
        <|> magic ammo ammoIcon ammoIcons
        <|> magic miningTools miningToolsIcon miningToolsIcons
        <|> magic car carIcon carIcons
        <|> magic tool toolIcon toolIcons
        <|> magic gun gunIcon gunIcons
        <|> magic module' moduleIcon moduleIcons
        <|> magic capsule capsuleIcon capsuleIcons
        <|> magic repairTool repairToolIcon repairToolIcons
        <|> magic armor armorIcon armorIcons
        <|> magic railPlanner railPlannerIcon railPlannerIcons
        <|> magic locomotive locomotiveIcon locomotiveIcons
        <|> magic fluidWagon fluidWagonIcon fluidWagonIcons
        <|> magic cargoWagon cargoWagonIcon cargoWagonIcons
        <|> magic artilleryWagon artilleryWagonIcon artilleryWagonIcons
      where
        magic
            :: Map String a
            -> (a -> Maybe String)
            -> (a -> Maybe (Map String IconPart))
            -> Maybe (Map String IconPart)
        magic x g h = ((flip M.lookup) x $ name) >>=
            (\v -> fmap toSingleIcon (g v) <|> h v)

run :: IO ()
run = do
    unzipImage
        "/home/yrid/.factorio/mods/angelsbioprocessing_0.5.9.zip"
        "graphics/icons/alien-bacteria.png"
        "angelsbioprocessing"
        "0.5.9"
    data' <- eitherDecodeStrict @RawData
        <$> B.readFile "purescript/data-raw-nice.json"

    either print (print . pairRecipeAndImages) data'
    either print (print . L.filter (M.null . icon) . pairRecipeAndImages) data'
    either print (print . length . L.filter (M.null . icon) . pairRecipeAndImages) data'
