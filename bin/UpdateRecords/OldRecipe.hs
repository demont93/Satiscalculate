{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module OldRecipe where


import           Data.Aeson
import qualified Data.Text as Text
import qualified Data.Vector as V
import           GHC.Generics



newtype Recipes a = Recipes { recipes :: V.Vector a }
             deriving (Show, Generic, FromJSON, ToJSON)


data Recipe = Recipe { opm :: Double
                     , name :: Text.Text
                     , craftingSeconds :: Double
                     , materialsNeeded :: V.Vector Material
                     }
            deriving (Show, Generic, FromJSON, ToJSON)




data Material = Material { name' :: Text.Text
                         , ipm :: Double
                         }
              deriving (Show, Generic)

instance FromJSON Material where
  parseJSON = withObject "Material" $ \v -> do
    name_ <- v .: "name"
    ipm_ <- v .: "ipm"
    return $ Material name_ ipm_


instance ToJSON Material where
  toJSON Material{..} =
    object [("name", toJSON name'), ("ipm", toJSON ipm)]
 

-- data NewRecipe = {Recipe}
