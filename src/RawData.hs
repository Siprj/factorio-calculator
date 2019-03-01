{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module RawData
    ( Ammo(..)
    , Armor(..)
    , ArtilleryWagon(..)
    , Capsule(..)
    , Car(..)
    , CargoWagon(..)
    , Fluid(..)
    , FluidWagon(..)
    , Gun(..)
    , IconPart(..)
    , Ingredient(..)
    , InputOutput(..)
    , Item(..)
    , ItemType(..)
    , Locomotive(..)
    , MiningTool(..)
    , Module(..)
    , RailPlanner(..)
    , RawData(..)
    , Recipe(..)
    , RepairTool(..)
    , Results(..)
    , Tool(..)
    )
  where

import Data.Aeson hiding (Result)
import Data.Aeson.TH
import Data.Char
import qualified Data.List as L
import Data.Map
import qualified Data.Map as M
import qualified Data.Text as T


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
        _ -> fail $ "ItemType should be fluid, item, ammo. Got: " <> T.unpack text
    parseJSON v = fail $ "ItemType should be string. Got: " <> show v

instance ToJSON ItemType where
    toJSON = \case
        ItemTypeFluid -> String "fuild"
        ItemTypeItem -> String "item"
        ItemTypeAmmo -> String "ammo"

data Ingredient = Ingredient
    { ingredientType :: ItemType
    , ingredientName :: String
    , ingredientAmount :: String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10} ''Ingredient)

data Results = Results
    { resultType :: Maybe ItemType -- Probably defaults to "item"
    , resultName :: String
    , resultAmount :: Maybe String
    , resultAmount_min :: Maybe String
    , resultAmount_max :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 6} ''Results)

data InputOutput = InputOutput
    { inputOutputIngredients :: Map String Ingredient
    , inputOutputResult :: Maybe String
    , inputOutputResults :: Maybe (Map String Ingredient)
    , inputOutputRequester_paste_multiplier :: Maybe String
    , inputOutputEnabled :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 11} ''InputOutput)

data Shift = Shift
    { _1 :: String
    , _2 :: String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = L.drop 1} ''Shift)

data IconPart = IconPart
    { iconPartIcon :: String
    , iconPartShift :: Maybe Shift
    , iconPartScale :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 8} ''IconPart)

data Recipe = Recipe
    { recipeType :: String
    , recipeName :: String
    , recipeCategory :: Maybe String
    , recipeNormal :: Maybe InputOutput
    , recipeExpensive :: Maybe InputOutput
    , recipeMain_product :: Maybe String
    , recipeIcon :: Maybe String
    , recipeIcons :: Maybe (Map String IconPart)
    , recipeIngredients :: Maybe (Map String Ingredient)
    , recipeResults :: Maybe (Map String Results)
    , recipeResult :: Maybe String
    , recipeSubgroup :: Maybe String
    , recipeOrder :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 6} ''Recipe)

data Item = Item
    { itemType :: ItemType
    , itemName :: String
    , itemIcon :: Maybe String
    , itemIcons :: Maybe (Map String IconPart)
    , itemSubgroup :: String
    , itemFuel_value :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 4} ''Item)

data Fluid = Fluid
    { fluidType :: String
    , fluidName :: String
    , fluidIcon :: Maybe String
    , fluidIcons :: Maybe (Map String IconPart)
    , fluidSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 5} ''Fluid)

data Ammo = Ammo
    { ammoType :: String
    , ammoName :: String
    , ammoIcon :: Maybe String
    , ammoIcons :: Maybe (Map String IconPart)
    , ammoSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 4} ''Ammo)

data MiningTool = MiningTool
    { miningToolsType :: String
    , miningToolsName :: String
    , miningToolsIcon :: Maybe String
    , miningToolsIcons :: Maybe (Map String IconPart)
    , miningToolsSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 11} ''MiningTool)

data Car = Car
    { carType :: String
    , carName :: String
    , carIcon :: Maybe String
    , carIcons :: Maybe (Map String IconPart)
    , carSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 3} ''Car)

data Tool = Tool
    { toolType :: String
    , toolName :: String
    , toolIcon :: Maybe String
    , toolIcons :: Maybe (Map String IconPart)
    , toolSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 4} ''Tool)

data Gun = Gun
    { gunType :: String
    , gunName :: String
    , gunIcon :: Maybe String
    , gunIcons :: Maybe (Map String IconPart)
    , gunSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 3} ''Gun)

data Module = Module
    { moduleType :: String
    , moduleName :: String
    , moduleIcon :: Maybe String
    , moduleIcons :: Maybe (Map String IconPart)
    , moduleSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 6} ''Module)

data Capsule = Capsule
    { capsuleType :: String
    , capsuleName :: String
    , capsuleIcon :: Maybe String
    , capsuleIcons :: Maybe (Map String IconPart)
    , capsuleSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 7} ''Capsule)

data RepairTool = RepairTool
    { repairToolType :: String
    , repairToolName :: String
    , repairToolIcon :: Maybe String
    , repairToolIcons :: Maybe (Map String IconPart)
    , repairToolSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10} ''RepairTool)

data Armor = Armor
    { armorType :: String
    , armorName :: String
    , armorIcon :: Maybe String
    , armorIcons :: Maybe (Map String IconPart)
    , armorSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 5} ''Armor)

data RailPlanner = RailPlanner
    { railPlannerType :: String
    , railPlannerName :: String
    , railPlannerIcon :: Maybe String
    , railPlannerIcons :: Maybe (Map String IconPart)
    , railPlannerSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 11} ''RailPlanner)

data Locomotive = Locomotive
    { locomotiveType :: String
    , locomotiveName :: String
    , locomotiveIcon :: Maybe String
    , locomotiveIcons :: Maybe (Map String IconPart)
    , locomotiveSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10} ''Locomotive)

data FluidWagon = FluidWagon
    { fluidWagonType :: String
    , fluidWagonName :: String
    , fluidWagonIcon :: Maybe String
    , fluidWagonIcons :: Maybe (Map String IconPart)
    , fluidWagonSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10} ''FluidWagon)

data CargoWagon = CargoWagon
    { cargoWagonType :: String
    , cargoWagonName :: String
    , cargoWagonIcon :: Maybe String
    , cargoWagonIcons :: Maybe (Map String IconPart)
    , cargoWagonSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 10} ''CargoWagon)


data ArtilleryWagon = ArtilleryWagon
    { artilleryWagonType :: String
    , artilleryWagonName :: String
    , artilleryWagonIcon :: Maybe String
    , artilleryWagonIcons :: Maybe (Map String IconPart)
    , artilleryWagonSubgroup :: Maybe String
    }
  deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap toLower . L.drop 14} ''ArtilleryWagon)
data RawData = RawData
    { recipe :: Map String Recipe
    , item :: Map String Item
    , fluid :: Map String Fluid
    , ammo :: Map String Ammo
    , miningTools :: Map String MiningTool
    , car :: Map String Car
    , tool :: Map String Tool
    , gun :: Map String Gun
    , module' :: Map String Module
    , capsule :: Map String Capsule
    , repairTool :: Map String RepairTool
    , armor :: Map String Armor
    , railPlanner :: Map String RailPlanner
    , locomotive :: Map String Locomotive
    , fluidWagon :: Map String FluidWagon
    , cargoWagon :: Map String CargoWagon
    , artilleryWagon :: Map String ArtilleryWagon
    }
  deriving (Show)

instance FromJSON RawData where
    parseJSON = withObject "RawData" $ \v -> RawData
        <$> v .: "recipe"
        <*> v .: "item"
        <*> v .: "fluid"
        <*> v .: "ammo"
        <*> v .: "mining-tool"
        <*> v .: "car"
        <*> v .: "tool"
        <*> v .: "gun"
        <*> v .: "module"
        <*> v .: "capsule"
        <*> v .: "repair-tool"
        <*> v .: "armor"
        <*> v .: "rail-planner"
        <*> v .: "locomotive"
        <*> v .: "fluid-wagon"
        <*> v .: "cargo-wagon"
        <*> v .: "artillery-wagon"
