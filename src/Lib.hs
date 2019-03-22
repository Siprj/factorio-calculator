{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( Configuration(..)
    , run
    )
  where

import Control.Applicative ((<|>), pure)
import Control.Monad
import Control.Monad.Error.Class (liftEither)
import Data.Aeson hiding (Result)
import Data.Aeson.TH
import Data.Char
import Data.Either
import Data.Functor
import qualified Data.Foldable as F
import Data.Maybe
import Data.Map
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Tuple.Extra
import qualified Data.Map as M
import qualified Data.List as L
import Codec.Archive.Zip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.Directory
import System.Environment (getArgs)
import System.FilePath
    ( (</>)
    , dropExtension
    , isExtensionOf
    , takeDirectory
    , takeFileName
    )

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

$(deriveJSON
    defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10}
    ''Ingredient)

data Result = Result
    { resultName :: String
    , resultAmount :: Int
    , resultProbablity :: Float
    }
  deriving (Show)

$(deriveJSON
    defaultOptions{fieldLabelModifier = fmap toLower . L.drop 6}
    ''Result)

data Shift = Shift
    { x :: Int
    , y :: Int
    }
  deriving (Show)

instance FromJSON Shift where
    parseJSON = withObject "Shift" $ \v -> Shift
        <$> v .: "1"
        <*> v .: "2"

instance ToJSON Shift where
    toJSON Shift{..} = object
        [ "x" .= x
        , "y" .= y
        ]

data IconPart = IconPart
    { iconPartIcon :: String
    , iconPartShift :: Maybe Shift
    , iconPartScale :: Maybe Float
    }
  deriving (Show)

$(deriveJSON
    defaultOptions{fieldLabelModifier = fmap toLower . L.drop 8}
    ''IconPart)

data Recipe = Recipe
    { name :: String
    , ingredients :: [Ingredient]
    , results :: [Result]
    , icon :: [IconPart]
    }
  deriving (Show)

$(deriveJSON defaultOptions ''Recipe)

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
modReference string = "__"
    <> (reverse . L.drop 1 . dropWhile ('_' /=) $ reverse string)
    <> "__"

associateMods :: FilePath -> FilePath -> IO ModAssociation
associateMods modsDir internalDir = do
    (zips, dirs) <- findMods modsDir
    let zipPairs = zip (modReference . dropExtension <$> zips)
            $ fmap (Zip . (modsDir </>)) zips
    pure . M.fromList
        $ zipPairs
        <> zip (fmap modReference dirs)
            (fmap (Directory . (modsDir </>)) dirs)
        <> internalMods internalDir

copyImages :: FilePath -> FilePath -> FilePath -> [Recipe] -> IO ()
copyImages modsDir internalDir dstDir recipes = do
    associatedMods <- associateMods modsDir internalDir
    mapM_ (copyImages' associatedMods) icons
  where
    icons = fmap splitModNameAndPath . mconcat
        $ fmap (fmap iconPartIcon . icon) recipes

    copyImages' associatedMods (name, path) =
        case associatedMods M.!? name of
            Just (Directory dir) -> do
                putStrLn $ "Copying from: " <> show (dir </> path) <> " to: "
                    <> dstPath
                createDirectoryIfMissing True $ takeDirectory dstPath
                copyFile (dir </> path) dstPath
            Just (Zip zipFile) -> do
                putStrLn $ "Unziping: " <> show zipFile <> " from: "
                    <> show path <> " to: " <> dstPath
                unzipImage zipFile path dstPath
            Nothing -> do
                print $ name <> "missing in associatedMods"
                print associatedMods
      where
        dstPath = dstDir </> name </> path

--pairRecipeAndImages :: RD.RawData -> [Recipe]
--pairRecipeAndImages rawData@RD.RawData{..} =
--    pairRecipeAndImage rawData <$> elems recipe

-- pairRecipeAndImage
--     :: RD.RawData
--     -> RD.Recipe
--     -> Recipe
-- pairRecipeAndImage RD.RawData{..} rec@RD.Recipe{..} = Recipe
--     { name = recipeName
--     , ingredients =
--         concatMap (fmap toIngredient . M.elems) recipeIngredients
--     , results =
--         rootResult <> F.concat (fmap (fmap toResult . M.elems) recipeResults)
--     , icon = M.elems . fromMaybe M.empty $
--         fmap toSingleIcon recipeIcon
--         <|> fmap (fmap toIconPart) recipeIcons
--         <|> resultIcon
--         <|> resultsIcon
--         <|> normalResultIcon
--         <|> normalResultsIcon
--     }
--   where
--     toIngredient :: RD.Ingredient -> Ingredient
--     toIngredient RD.Ingredient{..} = Ingredient
--         { ingredientName = ingredientName
--         , ingredientAmount = read ingredientAmount
--         }
--
--     rootResult :: [Result]
--     rootResult = F.toList $ do
--         name <- recipeResult
--         pure $ Result
--             { resultName = name
--             , resultAmount = maybe 1 read recipeResult_count
--             , resultProbablity = 1
--             }
--
--     toResult :: RD.Results -> Result
--     toResult RD.Results{..} = Result
--         { resultName = resultName
--         , resultAmount = maybe 1 read resultAmount
--         , resultProbablity = maybe 1 read resultProbablity
--         }
--
--     resultIcon :: Maybe (Map String IconPart)
--     resultIcon = recipeResult >>= getItemFluidIcon
--
--     resultsIcon :: Maybe (Map String IconPart)
--     resultsIcon = recipeResults
--         >>= (listToMaybe . elems)
--         >>= (\RD.Results{..} -> getItemFluidIcon resultName)
--
--     normalResultIcon :: Maybe (Map String IconPart)
--     normalResultIcon = recipeNormal
--         >>= RD.inputOutputResult >>= getItemFluidIcon
--
--     normalResultsIcon :: Maybe (Map String IconPart)
--     normalResultsIcon = recipeNormal
--         >>= RD.inputOutputResults
--         >>= (listToMaybe . elems)
--         >>= (\RD.Ingredient{..} -> getItemFluidIcon ingredientName)
--
--     toSingleIcon :: String -> Map String IconPart
--     toSingleIcon = M.singleton "1" . simpleIconPart
--
--     simpleIconPart v = IconPart v Nothing Nothing
--
--     getItemFluidIcon :: String -> Maybe (Map String IconPart)
--     getItemFluidIcon name = magic item RD.itemIcon RD.itemIcons
--         <|> magic fluid RD.fluidIcon RD.fluidIcons
--         <|> magic ammo RD.ammoIcon RD.ammoIcons
--         <|> magic miningTools RD.miningToolsIcon RD.miningToolsIcons
--         <|> magic car RD.carIcon RD.carIcons
--         <|> magic tool RD.toolIcon RD.toolIcons
--         <|> magic gun RD.gunIcon RD.gunIcons
--         <|> magic module' RD.moduleIcon RD.moduleIcons
--         <|> magic capsule RD.capsuleIcon RD.capsuleIcons
--         <|> magic repairTool RD.repairToolIcon RD.repairToolIcons
--         <|> magic armor RD.armorIcon RD.armorIcons
--         <|> magic railPlanner RD.railPlannerIcon RD.railPlannerIcons
--         <|> magic locomotive RD.locomotiveIcon RD.locomotiveIcons
--         <|> magic fluidWagon RD.fluidWagonIcon RD.fluidWagonIcons
--         <|> magic cargoWagon RD.cargoWagonIcon RD.cargoWagonIcons
--         <|> magic artilleryWagon RD.artilleryWagonIcon RD.artilleryWagonIcons
--       where
--         magic
--             :: Map String a
--             -> (a -> Maybe String)
--             -> (a -> Maybe (Map String RD.IconPart))
--             -> Maybe (Map String IconPart)
--         magic x g h = do
--             v <- M.lookup name x
--             fmap toSingleIcon (g v) <|> (fmap (fmap toIconPart) $ h v)

eitherToFail :: Either String a -> IO a
eitherToFail (Right a) = pure a
eitherToFail (Left e) = fail e

data Configuration = Configuration
    { rawDataPath :: FilePath
    , factorioModsDirPath :: FilePath
    , factorioDataDirPath :: FilePath
    , outputDir :: FilePath
    }

run :: Configuration -> IO ()
run Configuration{..} = do
    data' <- B.readFile rawDataPath
        >>= (eitherToFail . eitherDecodeStrict @RD.RawData)
    print data'
    mapM_ (\RD.Recipe{..} -> print $ show (length recipeResults) <> " " <> recipeName) . M.elems $ RD.recipe data'
--    data' <- B.readFile rawDataPath
--        >>= (eitherToFail . eitherDecodeStrict @RD.RawData)
--
--    let parsedData = pairRecipeAndImages data'
--    copyImages
--        factorioModsDirPath
--        factorioDataDirPath
--        outputDir
--        parsedData
--
--    BL.writeFile (outputDir </> "data.json") $ encode parsedData
