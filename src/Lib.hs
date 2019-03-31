{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

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

data Ingredient = Ingredient
    { ingredientName :: String
    , ingredientAmount :: Float
    }
  deriving (Show)

$(deriveJSON
    defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10}
    ''Ingredient)

data Product = Product
    { productName :: String
    , productAmount :: Float
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
    , recipeProducts :: [Product]
    , recipeIcon :: [IconPart]
    }
  deriving (Show)

instance ToJSON Recipe where
    toJSON Recipe{..} = object
        [ "name" .= recipeName
        , "ingredients" .= recipeIngredients
        , "products" .= recipeProducts
        , "icon" .= recipeIcon
        ]

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

instance ToJSON FactorioData where
    toJSON FactorioData{..} = object
        [ "items" .= items
        , "recipes" .= recipes
        ]

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

copyImages :: FilePath -> FilePath -> FilePath -> [IconPart] -> IO ()
copyImages modsDir internalDir dstDir icons = do
    associatedMods <- associateMods modsDir internalDir
    mapM_ (copyImages' associatedMods . splitModNameAndPath . iconPartIcon)
        icons
  where
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
toNiceFactorioData RD.FactorioData{..} = FactorioData
    { items = fmap toItem $ RD.rawItems raw
    , recipes = fmap toRecipe $ RD.inGameDataRecipes inGameData
    }
  where
    toRecipe RD.Recipe{..} = Recipe
        { recipeName
        , recipeIngredients = toIngredient <$> recipeIngredients
        , recipeProducts = toProduct <$> recipeProducts
        , recipeIcon = getRecipeIcon recipeName recipeProducts
        }

    toIngredient RD.Ingredient{..} = Ingredient
        { ingredientName
        , ingredientAmount
        }

    toProduct RD.Product{..} = Product
        { productName
        , productAmount = fromMaybe 1 productAmount
        , productProbablity = fromMaybe 1.0 productProbablity
        }

    -- TODO: lens may help to simplify this...
    getRecipeIcon recipeName recipeProducts = fromMaybe [] $
        (findRawRecipe recipeName >>= fmap (fmap toIconPart) . RD.rawRecipeIcons)
        <|> (fmap (fmap toIconPart . RD.rawItemIcons) $ findRawItem recipeName)
        <|> (RD.productName <$> listToMaybe recipeProducts >>= fmap (fmap toIconPart . RD.rawItemIcons) . findRawItem)

    findRawRecipe :: String -> Maybe RD.RawRecipe
    findRawRecipe recipeName = F.find
        (\RD.RawRecipe{..} -> recipeName == rawRecipeName) $ RD.rawRecipes raw

    findRawItem :: String -> Maybe RD.RawItem
    findRawItem recipeName =
        F.find (\RD.RawItem{..} -> recipeName == rawItemName) $ RD.rawItems raw

    toShift RD.Shift{..} = Shift
        { x
        , y
        }

    toIconPart RD.IconPart{..} = IconPart
        { iconPartIcon = iconPath
        , iconPartScale = scale
        , iconPartShift = fmap toShift shift
        }

    toItem RD.RawItem{..} = Item
        { itemName = rawItemName
        , itemIcon = fmap toIconPart rawItemIcons
        }

run :: Configuration -> IO ()
run Configuration{..} = do
    data' <- B.readFile rawDataPath
        >>= (eitherToFail . eitherDecodeStrict @RD.FactorioData)

    let niceData = toNiceFactorioData data'
    let itemIcons = mconcat . fmap itemIcon $ items niceData
    let recipeIcons = mconcat . fmap recipeIcon $ recipes niceData
    copyImages factorioModsDirPath factorioDataDirPath outputDir
        $ itemIcons <> recipeIcons

    BL.writeFile (outputDir </> "data.json") $ encode niceData
