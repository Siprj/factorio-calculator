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

data Ingredient = Product
    { ingredientName :: String
    , ingredientAmount :: Int
    }
  deriving (Show)

$(deriveJSON
    defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10}
    ''Ingredient)

data Product = Product
    { productName :: String
    , productAmount :: Int
    , productProbablity :: Float
    }
  deriving (Show)

$(deriveJSON
    defaultOptions{fieldLabelModifier = fmap toLower . L.drop 7}
    ''Product)

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
    { recipeName :: String
    , recipeIngredients :: [Ingredient]
    , recipeResults :: [Product]
    , recipeIcon :: [IconPart]
    }
  deriving (Show)

$(deriveJSON defaultOptions ''Recipe)

data Item = Item
    { itemName :: String
    , itemIcon :: [IconPart]
    }
  deriving (Show)

instance ToJSON Item where
    toJSON Item{..} = object
        [ "name" .= itemName
        , "icon" .= itemIcon
        ]

data FactorioData = FactorioData
    { items :: [Item]
    , recipes :: [Recipe]
    }
  deriving (Show)

data ModType
    = Zip FilePath
    | Directory FilePath
  deriving (Show)

type ModAssociation = Map String ModType

data Configuration = Configuration
    { rawDataPath :: FilePath
    , factorioModsDirPath :: FilePath
    , factorioDataDirPath :: FilePath
    , outputDir :: FilePath
    }

unzipImage :: ZipPath -> FilePath -> FilePath -> IO ()
unzipImage zipPath file destination = do
    createDirectoryIfMissing True $ takeDirectory destination
    mkEntrySelector filePhatInZip
        >>= withArchive zipPath . getEntry
        >>= B.writeFile destination
  where
    filePhatInZip = dropExtension (takeFileName zipPath) </> file

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

eitherToFail :: Either String a -> IO a
eitherToFail (Right a) = pure a
eitherToFail (Left e) = fail e

toNiceFactorioData :: RD.FactorioData -> FactorioData
toNiceFactorioData RD.FactorioData{..} =
  where
    toRecipe RD.Recipe = Recipe
        { recipeName
        , recipeIngredients = toIngredient <$> recipeIngredients
        , recipeProducts =

    toIngredient RD.Ingredient{..} = Ingredient
        { ingredientName
        , ingredientAmount
        }

    toResult RD.Product{..} = Product
        { productName
        , productAmount = fromMaybe 1 productAmount
        , productProbablity = formMaybe 1.0 productProbablity
        }

run :: Configuration -> IO ()
run Configuration{..} = do
    data' <- B.readFile rawDataPath
        >>= (eitherToFail . eitherDecodeStrict @RD.FactorioData)
    print data'
    -- mapM_ (\RD.Recipe{..} -> print $ show (length recipeResults) <> " " <> recipeName) . M.elems $ RD.recipe data'
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
