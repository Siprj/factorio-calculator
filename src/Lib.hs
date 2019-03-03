{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( ZipPath
    , unzipImage
    , run
    , Recipe(..)
    , findMods
    , modReference
    ) where

import Control.Applicative ((<|>), pure)
import Control.Monad
import Control.Monad.Error.Class (liftEither)
import Data.Aeson hiding (Result)
import Data.Char
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Tuple.Extra
import qualified Data.Map as M
import qualified Data.List as L
import Codec.Archive.Zip
import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.FilePath ((</>), takeDirectory, isExtensionOf, dropExtension, takeFileName)
import System.Directory

import RawData hiding (Ingredient)
import qualified RawData as RD


type ZipPath = FilePath

unzipImage :: ZipPath -> FilePath -> FilePath -> IO ()
unzipImage zipPath file destination = do
    createDirectoryIfMissing True $ takeDirectory destination
    mkEntrySelector filePhatInZip
        >>= withArchive zipPath . getEntry
        >>= B.writeFile destination
  where
    filePhatInZip = dropExtension (takeFileName zipPath) </> file

data Ingredient = Ingredient
    { ingredientName :: String
    , ingredientAmount :: Int
    }
  deriving (Show)

data Result = Result
    { resultName :: String
    , resultAmount :: Int
    , resultProbablity :: Float
    }

data RecipeWithImage = RecipeWithImage
    { name :: String
    , ingredients :: [Ingredient]
    , icon :: Map String IconPart
    }
  deriving (Show)

data ModType
    = Zip FilePath
    | Directory FilePath
  deriving (Show)

type ModAssociation = Map String ModType

internalMods :: FilePath -> [(String, ModType)]
internalMods path = fmap (fmap (Directory . (path </>)))
    [("__base__", "base"), ("__core__", "core")]

splitModNameAndPath :: String -> (String, FilePath)
splitModNameAndPath = fmap (L.drop 1) . break (== '/')

findMods :: String -> IO ([FilePath], [FilePath])
findMods modDir = do
    list <- listDirectory modDir
    dirs <- filterM (\d -> doesDirectoryExist $ modDir </> d)  list
    return (L.filter (isExtensionOf "zip") list, dirs)

maybeToRight :: e -> Maybe a -> Either e a
maybeToRight e = maybe (Left e) Right

modReference :: String -> String
modReference string = "__" <> (reverse . L.drop 1 . snd . break ('_' ==) $ reverse string)  <> "__"

associateMods :: FilePath -> FilePath -> IO ModAssociation
associateMods modsDir internalDir = do
    (zips, dirs) <- findMods modsDir
    let zipPairs = zip (modReference . dropExtension <$> zips) $ fmap (Zip . (modsDir </>)) zips
    pure . M.fromList
        $ zipPairs
        <> (zip (fmap (modReference) dirs) $ fmap (Directory . (modsDir </>)) dirs)
        <> internalMods internalDir

copyImages :: FilePath -> FilePath -> FilePath -> [RecipeWithImage] -> IO ()
copyImages modsDir internalDir dstDir recipes = do
    putStrLn $ "dstDir: " <> show dstDir
    associatedMods <- associateMods modsDir internalDir
    mapM_ (copyImages' associatedMods) icons
  where
    icons = fmap splitModNameAndPath . mconcat
        $ fmap (fmap iconPartIcon . elems . icon) recipes

    copyImages' associatedMods (name, path) = do
        putStrLn $ "asdfasdf2aasdasdfasdf: " <> name </> L.drop 1 path
        case associatedMods M.!? name of
            Just (Directory dir) -> do
                putStrLn $ "Copying from: " <> show (dir </> path) <> " to: " <> dstPath
                createDirectoryIfMissing True $ takeDirectory dstPath
                copyFile (dir </> path) $ dstPath
            Just (Zip zipFile) -> do
                putStrLn $ "Unziping: " <> show zipFile <> " from: " <> show path <> " to: " <> dstPath
                unzipImage zipFile path dstPath
            Nothing -> do
                print $ name <> "missing in associatedMods"
                print associatedMods
      where
        dstPath = dstDir </> name </> path

pairRecipeAndImages :: RawData -> [RecipeWithImage]
pairRecipeAndImages rawData@RawData{..} =
    fmap (pairRecipeAndImage rawData) $ elems recipe

pairRecipeAndImage
    :: RawData
    -> Recipe
    -> RecipeWithImage
pairRecipeAndImage RawData{..} Recipe{..} = RecipeWithImage
    { name = recipeName
    , ingredients = maybe [] (fmap toIngredient . M.elems) recipeIngredients
    , icon = fromMaybe M.empty $
        fmap toSingleIcon recipeIcon
        <|> recipeIcons
        <|> resultIcon
        <|> resultsIcon
        <|> normalResultIcon
        <|> normalResultsIcon
    }
  where
    toIngredient :: RD.Ingredient -> Ingredient
    toIngredient RD.Ingredient{..} = Ingredient
        { ingredientName = ingredientName
        , ingredientAmount = read ingredientName
        }

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
        >>= (\RD.Ingredient{..} -> getItemFluidIcon ingredientName)

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

eitherToFail :: Either String a -> IO a
eitherToFail (Right a) = pure a
eitherToFail (Left e) = fail e

run :: IO ()
run = do
    data' <- B.readFile "purescript/data-raw-nice.json"
        >>= (eitherToFail . eitherDecodeStrict @RawData)

    copyImages
        "/home/yrid/.factorio/mods"
        "/home/yrid/.steam/steam/steamapps/common/Factorio/data"
        "/home/yrid/factorio-images/"
        $ pairRecipeAndImages data'
