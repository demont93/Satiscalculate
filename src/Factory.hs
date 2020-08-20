module Factory (
  Factory(..),
  Dimensions(..),
  MaterialsNeeded(..),
  ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           GHC.Generics

data Factory
  = Factory
  { name :: Text.Text
  , factoryDimensions :: Dimensions
  , materialsRequired :: MaterialsNeeded
  , inputs            :: Int
  , outputs           :: Int }
  deriving (Show, Generic, FromJSON, ToJSON)

data Dimensions
  = Dimensions
  { width  :: Double
  , length :: Double
  , height :: Double
  }
  deriving (Show, ToJSON, FromJSON, Generic)

newtype MaterialsNeeded
  = MaterialsNeeded (Map.HashMap Text.Text Text.Text)
  deriving (Show, Generic)
  deriving newtype (FromJSON, ToJSON)
