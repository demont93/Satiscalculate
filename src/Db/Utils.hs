module Db.Utils (
  addManyRecipesToDbWithUserInput,
  addManyFactoriesTodbWithUserInput
  ) where

import           Cli
import           Cli.YesNo
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import           Db
import qualified Factory as F
import           Recipe

-- TODO remove duplication
addManyFactoriesTodbWithUserInput :: Db -> IO Db
addManyFactoriesTodbWithUserInput db = do
  maybeNewDb <- runMaybeT $! addFactoryToDb db
  case maybeNewDb of
    Nothing     -> return db
    Just newDb -> do
      yn <- askYesOrNo "Add another factory? (y/n)"
      case yn of
        Yes -> addManyFactoriesTodbWithUserInput newDb
        No  -> return db

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

addFactoryToDb :: Db -> MaybeT IO Db
addFactoryToDb db = do
  factory <- new
  let newDb = dbInsert (F.name factory) factory db
  liftIO $ writeDb newDb
  return newDb


addRecipeToDb :: Db -> MaybeT IO Db
addRecipeToDb db = do
  recipe <- new
  let newDb = dbInsert (name recipe) recipe db
  liftIO $ do
    writeDb newDb
    addMissingMaterialsToDb newDb (materialsNeeded recipe)

addRecipeWithNameToDb :: Db -> Text.Text -> MaybeT IO Db
addRecipeWithNameToDb db name' = do
  recipe <- newNamed name'
  let newDb = dbInsert (name recipe) recipe db
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
            itemToAdd <- MaybeT $ return $!
              case (dbLookup materialName db' :: Maybe Recipe) of
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
