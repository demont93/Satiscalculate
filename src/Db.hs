module Db (
  Db,
  recipeFromKey,
  insertRecipe,
  writeDb,
  loadDb
  ) where

import Control.Applicative ((<|>))
import           Data.Aeson
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import           GHC.Generics
import           Recipe

newtype Db
  = Db (Map.HashMap Text.Text Table)
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Table
  = FactoryTable (Map.HashMap Text.Text Factory)
  | RecipeTable (Map.HashMap Text.Text Recipe)
  deriving (Show, Generic)

-- TODO Refactor: code duplication
instance FromJSON Table where
  parseJSON = withArray "TableValues" $ \v ->
    (RecipeTable <$> V.foldM c Map.empty v)
    <|>
    (FactoryTable <$> V.foldM c' Map.empty v)
    where c k x = do
            r <- parseJSON x
            return $ Map.insert (Recipe.name r) r k
          c' k x = do
            f <- parseJSON x
            return $ Map.insert (Db.name f) f k

-- TODO Refactor: code duplication
instance ToJSON Table where
  toJSON (FactoryTable m) =
    toJSON $! V.fromList $ snd <$> Map.toList m
  toJSON (RecipeTable m)  =
    toJSON $! V.fromList $ snd <$> Map.toList m

data Factory
  = Factory
  { name :: Text.Text
  , factoryDimensions :: Dimensions
  , materialsRequired :: Map.HashMap Text.Text Text.Text
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

getTable :: Text.Text -> Db -> Maybe Table
getTable t (Db db) = Map.lookup t db

recipeFromKey :: Text.Text -> Db -> Maybe Recipe
recipeFromKey t (getTable "recipes" -> Just (RecipeTable m)) = Map.lookup t m
recipeFromKey _ _ = Nothing

insertRecipe :: Text.Text -> Recipe -> Db -> Db
insertRecipe k newRecipe (Db db) = Db $
  Map.alter c "recipes" db
  where c (Just (RecipeTable rt)) = Just $ RecipeTable $ Map.insert k newRecipe rt
        c Nothing                 = Just $ RecipeTable $ Map.singleton k newRecipe
        c (Just _)                = error "Something went terribly wrong."

writeDb :: Db -> IO ()
writeDb = encodeFile "db/db.json"

loadDb :: String -> IO (Either String Db)
loadDb = eitherDecodeFileStrict
