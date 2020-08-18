{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module NewRecipe where


import Data.Maybe (fromJust)
import           Data.Aeson
import qualified Data.HashMap.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import           GHC.Generics
import qualified OldRecipe as Old



newtype Recipes = Recipes { recipes :: V.Vector Recipe }
             deriving (Show, Generic, FromJSON, ToJSON)


data Recipe = Recipe { opm :: Double
                     , name :: Text.Text
                     , craftingSeconds :: Double
                     , materialsNeeded :: V.Vector Old.Material
                     , factoryName :: Text.Text
                     }
            deriving (Show, Generic, FromJSON, ToJSON)



specialFactories :: Map.HashMap Text.Text Text.Text
specialFactories = Map.fromList
  [ ("Iron Ingot", "Smelter")
  , ("Copper Ingot", "Smelter")
  , ("Caterium Ingot", "Smelter")
  , ("Steel Ingot", "Foundry")
  , ("Aluminum Ingot", "Foundry")
  ]


convert :: Old.Recipe -> Recipe
convert or
  | (Old.name or) `Map.member` specialFactories =
    addFactory $ fromJust $ Map.lookup (Old.name or) $ specialFactories
  | len == 1 = addFactory "Constructor"
  | len == 2 = addFactory "Assembler"
  | len == 3 = addFactory "Manufacturer"
  | len == 4 = addFactory "Manufacturer"
  | otherwise = error ""
  where len = V.length (Old.materialsNeeded or)
        addFactory f = Recipe
          (Old.opm or)
          (Old.name or)
          (Old.craftingSeconds or)
          (Old.materialsNeeded or)
          f
