module Db (
  Db,
  DbStorable(..),
  writeDb,
  loadDb,
  ) where

import qualified Factory as F
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
  = FactoryTable (Map.HashMap Text.Text F.Factory)
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
            return $ Map.insert (F.name f) f k

-- TODO Refactor: code duplication
instance ToJSON Table where
  toJSON (FactoryTable m) =
    toJSON $! V.fromList $ snd <$> Map.toList m
  toJSON (RecipeTable m)  =
    toJSON $! V.fromList $ snd <$> Map.toList m

class DbStorable e where
  dbInsert :: Text.Text -> e -> Db -> Db
  dbLookup :: Text.Text -> Db -> Maybe e

instance DbStorable Recipe where
  dbInsert = insertRecipe
  dbLookup = recipeFromKey

instance DbStorable F.Factory where
  dbLookup t (getTable "factories" -> Just (FactoryTable m)) = Map.lookup t m
  dbLookup _ _ = Nothing

  dbInsert k newFactory (Db db) = Db $
    Map.alter c "factories" db
    where c (Just (FactoryTable rt)) = Just $ FactoryTable $ Map.insert k newFactory rt
          c Nothing                  = Just $ FactoryTable $ Map.singleton k newFactory
          c (Just _)                 = error "Something went terribly wrong."


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
