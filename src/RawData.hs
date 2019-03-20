{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module RawData
    ( IconPart(..)
    , Ingredient(..)
    , InputOutput(..)
    , ItemType(..)
    , RawData(..)
    , Recipe(..)
    , Results(..)
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


data ItemType
    = ItemTypeAmmo
    | ItemTypeItem
    | ItemTypeFluid
    | ItemTypeRecipe
  deriving (Show)

instance FromJSON ItemType where
    parseJSON (String text) = case text of
        "fluid" -> pure ItemTypeFluid
        "item" -> pure ItemTypeItem
        "ammo" -> pure ItemTypeAmmo
        _ -> fail $ "ItemType should be fluid, item, ammo. Got: "
            <> T.unpack text
    parseJSON v = fail $ "ItemType should be string. Got: " <> show v

instance ToJSON ItemType where
    toJSON = \case
        ItemTypeFluid -> String "fuild"
        ItemTypeItem -> String "item"
        ItemTypeAmmo -> String "ammo"

data Ingredient = Ingredient
    { ingredientName :: String
    , ingredientAmount :: Int
    }
  deriving (Show)

fromArray :: FromJSON a => Array -> Int -> Parser a
fromArray a n = maybe fail' parseJSON $ a V.!? n
  where
    fail' = fail $ "Can't parse ingredient."

instance FromJSON Ingredient where
    parseJSON (Object o) = Ingredient
        <$> o .: "name"
        <*> o .: "amount"
    parseJSON (Array a) = Ingredient
        <$> fromArray a 0
        <*> fromArray a 1
    parseJSON v = typeMismatch "Ingredient" v

data Results = Results
    { resultType :: Maybe ItemType -- Probably defaults to "item"
    , resultName :: String
    , resultProbablity :: Maybe Float
    , resultAmount :: Maybe Int
    , resultAmount_min :: Maybe Float
    , resultAmount_max :: Maybe Float
    }
  deriving (Show)

instance FromJSON Results where
    parseJSON (Object o) = Results
        <$> o .:? "type"
        <*> o .: "name"
        <*> o .:? "probability"
        <*> o .:? "amount"
        <*> o .:? "amount_min"
        <*> o .:? "amount_max"
    parseJSON (Array a) = do
        name <- fromArray a 0
        amount <- fromArray a 1
        pure $ Results Nothing name Nothing amount Nothing Nothing
    parseJSON v = typeMismatch "Results" v

data InputOutput = InputOutput
    { inputOutputIngredients :: V.Vector Ingredient
    , inputOutputResult :: Maybe String
    , inputOutputResults :: Maybe (Map String Ingredient)
    , inputOutputRequester_paste_multiplier :: Maybe String
    , inputOutputEnabled :: Maybe Bool
    }
  deriving (Show)

instance FromJSON InputOutput where
    parseJSON = withObject "InputOutput" $ \v -> InputOutput
        <$> v .: "ingredients"
        <*> v .:? "result"
        <*> v .:? "results"
        <*> v .:? "requester_paste_multiplier"
        <*> v .:? "enabled"

data Recipe = Recipe
    { recipeType :: String
    , recipeName :: String
    , recipeCategory :: Maybe String
    , recipeNormal :: Maybe InputOutput
    , recipeExpensive :: Maybe InputOutput
    , recipeMain_product :: Maybe String
    , recipeIcons :: Maybe [IconPart]
    , recipeIngredients :: Maybe (V.Vector Ingredient)
    , recipeResults :: Maybe (V.Vector Results)
    , recipeResult :: Maybe String
    , recipeResult_count :: Maybe Int
    , recipeSubgroup :: Maybe String
    , recipeOrder :: Maybe String
    , recipeEnergy_required :: Maybe Float
    }
  deriving (Show)

instance FromJSON Recipe where
    parseJSON = withObject "Recipe" $ \v -> Recipe
        <$> v .: "type"
        <*> v .: "name"
        <*> v .:? "category"
        <*> v .:? "normal"
        <*> v .:? "expensive"
        <*> v .:? "main_product"
        <*> getIcons v
        <*> v .:? "ingredients"
        <*> v .:? "results"
        <*> v .:? "result"
        <*> v .:? "result_count"
        <*> v .:? "subgroup"
        <*> v .:? "order"
        <*> v .:? "energy_required"
      where
        getIcons :: Object -> Parser (Maybe [IconPart])
        getIcons v = fmap (pure . pure . createIcon) (v .: "icon")
            <|> (fmap pure $ v .: "icons")
            <|> pure Nothing

        createIcon p = IconPart p Nothing Nothing

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
    parseJSON = parseJSON >=> fromArray
      where
        fail' = fail $ "Can't parse shift."

        fromArray :: V.Vector Int -> Parser Shift
        fromArray a = maybe fail' pure $ do Shift
            <$> a V.!? 0
            <*> a V.!? 1

data Item = Item
    { item2Name :: String
    , item2Type :: String
    , item2Icons :: [IconPart]
    }
  deriving (Show)

instance FromJSON Item where
    parseJSON = withObject "Item" $ \v -> do
        name' <- v .: "name"
        type' <- v .: "type"
        icons' <- getIcons v
        pure $ Item name' type' icons'
      where
        getIcons :: Object -> Parser [IconPart]
        getIcons v = fmap (pure . createIcon) (v .: "icon")
            <|> v .: "icons"

        createIcon p = IconPart p Nothing Nothing

data RawData = RawData
    { recipe :: Map String Recipe
    , items :: [Item]
    }
  deriving (Show)

instance FromJSON RawData where
    parseJSON = withObject "RawData" $ \v -> RawData
        <$> v .: "recipe"
        <*> (fmap mconcat . sequence . fmap magic $ getObjects v)
      where
        magic :: Value -> Parser [Item]
        magic v = fmap M.elems (parseJSON v :: Parser (Map String Item))
            <|> pure []

        getObjects v = mconcat $ fmap (maybeToList . flip HM.lookup v) keys
        keys =
            [ "recipe"
            , "item"
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

