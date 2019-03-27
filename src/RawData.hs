{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RawData
    ( IconPart(..)
    , Ingredient(..)
    , FactorioData(..)
    , Recipe(..)
    , Shift(..)
    , Item(..)
    )
  where

import Control.Applicative ((<|>), many)
import Control.Monad ((>=>))
import Data.Aeson hiding (Result)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Char
import qualified Data.List as L
import Data.Map
import Data.Maybe
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V


fromArray :: FromJSON a => Array -> Int -> String -> Parser a
fromArray a n name = maybe fail' parseJSON $ a V.!? n
  where
    fail' = fail $ "Can't parse " <> name <> "."

data Ingredient = Ingredient
    { ingredientName :: String
    , ingredientAmount :: Int
    }
  deriving (Show)

instance FromJSON Ingredient where
    parseJSON = withObject "Ingredient" $ \o -> Ingredient
        <$> o .: "name"
        <*> o .: "amount"

data Product = Product
    { resultName :: String
    , resultAmount :: Maybe Int
    , resultProbablity :: Maybe Float
    , resultAmount_min :: Maybe Float
    , resultAmount_max :: Maybe Float
    }
  deriving (Show)

instance FromJSON Product where
    parseJSON = withObject "Product" $ \o -> Product
        <$> o .: "name"
        <*> o .:? "amount"
        <*> o .:? "probability"
        <*> o .:? "amount_min"
        <*> o .:? "amount_max"

data Recipe = Recipe
    { recipeName :: String
    , recipeCategory :: String
    , recipeIngredients :: [Ingredient]
    , recipeProducts :: [Product]
    , recipeEnergy :: Float
    }
  deriving (Show)

instance FromJSON Recipe where
    parseJSON = withObject "Recipe" $ \o -> Recipe
        <$> o .: "name"
        <*> o .: "category"
        <*> o .: "ingredients"
        <*> o .: "products"
        <*> o .: "energy"

data IconPart = IconPart
    { iconPath :: String
    , scale :: Maybe Float
    , shift :: Maybe Shift
    }
  deriving (Show)

instance FromJSON IconPart where
    parseJSON = withObject "IconPart" $ \v -> IconPart
        <$> v .: "icon"
        <*> v .:? "scale"
        <*> v .:? "shift"

data Shift = Shift
    { x2 :: Int
    , y2 :: Int
    }
  deriving (Show)

instance FromJSON Shift where
    parseJSON v = pure $ Shift 0 0
        -- parseJSON >=> fromArray
      where
        fail' = fail $ "Can't parse shift."

        fromArray :: V.Vector Int -> Parser Shift
        fromArray a = maybe fail' pure $ do Shift
            <$> a V.!? 0
            <*> a V.!? 1

data Item = Item
    { item2Name :: String
    , item2Icons :: [IconPart]
    }
  deriving (Show)

instance FromJSON Item where
    parseJSON = withObject "Item" $ \v -> do Item
        <$> v .: "name"
        <*> v .: "icons"

data Items = Items
    { itemsItems :: [Item]
    }
  deriving (Show)

instance FromJSON Items where
    parseJSON = withObject "Items" $ \o -> Items
        <$> (fmap mconcat . sequence . fmap magic $ getObjects o)
      where
        magic :: Value -> Parser [Item]
        magic o = fmap M.elems (parseJSON o :: Parser (Map String Item))
            <|> pure []

        getObjects o = mconcat $ fmap (maybeToList . flip HM.lookup o) keys
        keys =
            [ "item"
            , "fluid"
            , "ammo"
            , "mining-tool"
            , "car"
            , "tool"
            , "gun"
            , "module"
            , "capsule"
            , "repair-tool"
            , "armor"
            , "rail-planner"
            , "locomotive"
            , "fluid-wagon"
            , "cargo-wagon"
            , "artillery-wagon"
            ]

data RawRecipe = RawRecipe
    { rawRecipeName :: String
    , rawRecipeIcons :: [IconPart]
    }
  deriving (Show)

instance FromJSON RawRecipe where
    parseJSON = withObject "RawRecipe" $ \o -> RawRecipe
        <$> o .: "name"
        <*> o .: "icons"

data RawItem = RawItem
    { rawItemName :: String
    , rawItemIcons :: [IconPart]
    }
  deriving (Show)

instance FromJSON RawItem where
    parseJSON = withObject "RawItem" $ \o -> RawItem
        <$> o .: "name"
        <*> o .: "icons"

data Raw = Raw
    { rawRecipes :: [RawRecipe]
    , rawItems :: [RawItem]
    }
  deriving (Show)

instance FromJSON Raw where
    parseJSON = withObject "Raw" $ \o -> Raw
        <$> o .: "recipeIcons"
        <*> o .: "itemIcons"

data InGame = InGame
    { inGameDataRecipes :: [Recipe]
    }
  deriving (Show)

instance FromJSON InGame where
    parseJSON = withObject "InGame" $ \o -> InGame
        <$> o .: "inGameRecipes"

data FactorioData = FactorioData
    { raw :: Items
    , inGameData :: InGame
    }
  deriving (Show)

instance FromJSON FactorioData where
    parseJSON = withObject "FactorioData" $ \v -> FactorioData
        <$> v .: "raw"
        <*> v .: "inGameData"
