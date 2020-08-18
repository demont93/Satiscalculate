module Cli (
  askData,
  confirmInput,
  genRecipe,
  genRecipeWithName
  ) where

import           Cli.Qna
import           Cli.ValidInput
import           Cli.YesNo
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict       as Map
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text.IO
import           Recipe

askData :: ValidInput b => Text.Text -> Text.Text -> MaybeT QnA b
askData q n = do
  v <- liftIO $ do
    Text.IO.putStrLn $ q <> ":  "
    Text.IO.getLine
  lift $ recordInput (UserInteraction n v)
  MaybeT $ return $ parseUserInput v

confirmInput :: QnA a -> MaybeT QnA a
confirmInput r = do
  ui <- lift $! listenInput r
  MaybeT $ liftIO $ do
    Text.IO.putStrLn $ formatHistory $ vna ui
    yn <- askYesOrNo "Is this correct? (y/n)"
    case yn of
      No  -> return Nothing
      Yes -> return $! Just $ value ui

formatHistory :: [UserInteraction] -> Text.Text
formatHistory = foldr c ""
  where c (UserInteraction name answer) acc =
          name <> ":  " <> answer <> "\n" <> acc

genRecipe :: MaybeT QnA Recipe
genRecipe = askData "Enter exact item name" "Name" >>= genRecipeWithName

genRecipeWithName :: Text.Text -> MaybeT QnA Recipe
genRecipeWithName name' = do
  opm' <- askData "Enter recipe output per minute" "Output per minute"
  craftingSeconds' <- askData "Enter recipe crafting time in seconds" "Crafting seconds"
  factoryName' <- askData "Enter factory used to build this item" "Factory name"
  materialsNeeded' <- getMaterialsNeeded Map.empty
  return Recipe { name = name'
                , opm  = opm'
                , craftingSeconds = craftingSeconds'
                , materialsNeeded = materialsNeeded'
                , factoryName     = factoryName'
                }

getMaterialsNeeded :: Map.HashMap Text.Text Double -> MaybeT QnA (Map.HashMap Text.Text Double)
getMaterialsNeeded m = do
  name' <- askData "Enter name of new material needed" "Material Name"
  ipm' <- askData "Enter input per minute requirement for the material" "Input per Minute"
  let map'= Map.insert name' ipm' m
  yn <- liftIO $ askYesOrNo "Do you want to add another material? (y/n)"
  case yn of
    No  -> return map'
    Yes -> getMaterialsNeeded map'
