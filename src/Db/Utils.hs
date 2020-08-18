module Db.Utils (
  addManyRecipesToDbWithUserInput
  ) where


import           Cli
import           Cli.Qna
import           Cli.YesNo
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Db
import           Recipe


addManyFactoriesTodbWithUserInput :: Db -> IO Db
addManyFactoriesTodbWithUserInput = undefined

addManyRecipesToDbWithUserInput :: Db -> IO Db
addManyRecipesToDbWithUserInput db2 = do
  maybeNewDb2 <- runMaybeT $! addRecipeToDb db2
  case maybeNewDb2 of
    Nothing     -> return db2
    Just newDb2 -> do
      yn <- askYesOrNo "Add another recipe? (y/n)"
      case yn of
        Yes -> addManyRecipesToDbWithUserInput newDb2
        No  -> return db2


addRecipeToDb :: Db -> MaybeT IO Db
addRecipeToDb db = do
  recipe <- newRecipe
  let newDb = insertRecipe (name recipe) recipe db
  liftIO $ do
    writeDb newDb
    addMissingMaterialsToDb newDb (materialsNeeded recipe)


addRecipeWithNameToDb :: Db -> Text.Text -> MaybeT IO Db
addRecipeWithNameToDb db name' = do
  recipe <- newRecipeWithName name'
  let newDb = insertRecipe (name recipe) recipe db
  liftIO $ do
    writeDb newDb
    addMissingMaterialsToDb newDb (materialsNeeded recipe)


addMissingMaterialsToDb :: Db -> Map.HashMap Text.Text Double -> IO Db
addMissingMaterialsToDb initialDb = Map.foldlWithKey' c $ return initialDb
  where
    c :: IO Db -> Text.Text -> Double -> IO Db
    c db materialName _ = do
          db' <- db
          res <- runMaybeT $ do
            itemToAdd <- MaybeT $ return $! case recipeFromKey db' materialName of
              Nothing           -> Just materialName
              Just _alreadyInDb -> Nothing
            liftIO $ Text.IO.putStrLn $ "Adding Recipe: " <> itemToAdd
            addMaterialToDb db' itemToAdd
          return $! fromMaybe db' res


addMaterialToDb :: Db -> Text.Text -> MaybeT IO Db
addMaterialToDb db name' = do
  yn <- liftIO $ askYesOrNo $ "Is this a Base Material (i.e. is not " <>
                       "composed of any other materials) (y/n)"
  case yn of
    Yes -> empty
    No  -> addRecipeWithNameToDb db name'




newRecipe :: MaybeT IO Recipe
newRecipe = MaybeT $ join
  <$> getAnswers (runMaybeT $ confirmInput $ runMaybeT genRecipe)


newRecipeWithName :: Text.Text -> MaybeT IO Recipe
newRecipeWithName name = MaybeT $ join
  <$> getAnswers (runMaybeT $ confirmInput $ runMaybeT $ genRecipeWithName name)
