module Db
  ( Db
  , recipeFromKey
  , insertRecipe
  -- , writeDb
  -- , loadDb
  ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import           Recipe

newtype Db = Db (Map.HashMap Text.Text Table)
           deriving Show

newtype Table = Table (Map.HashMap Text.Text Record)
              deriving Show

data Record
  = RecipeRecord Recipe
  | FactoryRecord Factory
  deriving Show

data Factory = Factory Int Double
             deriving Show

instance ToJSON Db where
  toJSON (Db db) = object ["recipes" .= recipes]
    where recipes = V.fromList $ Map.foldr (\v acc -> toJSON v : acc) [] db

-- instance FromJSON Db where
--   parseJSON = withObject "recipe list" $ \v -> do
--     vec_ <- v .: "recipes"
--     return $ Db $
--       V.foldl'
--       (\acc cur@(Recipe name_ _ _ _ _) -> Map.insert name_ cur acc)
--       Map.empty
--       vec_

recipeFromKey :: Db -> Text.Text -> Maybe Recipe
recipeFromKey (Db db) n =
  Map.lookup "recipe" db >>=
  \(Table x) -> (Map.lookup n x) >>=
  \(RecipeRecord r) -> return r

insertRecipe :: Text.Text -> Recipe -> Db -> Db
insertRecipe k v (Db db) = Db $!
  Map.alter c "recipe" db
  where c (Just (Table old)) = Just $ Table $! Map.insert k (RecipeRecord v) old
        c Nothing            = Just $ Table $! Map.singleton k (RecipeRecord v)

-- writeDb :: Db -> IO ()
-- writeDb = encodeFile "db/Recipes.json"

-- loadDb :: String -> IO (Either String Db)
-- loadDb = eitherDecodeFileStrict
