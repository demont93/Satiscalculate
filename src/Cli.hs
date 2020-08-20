module Cli (
  askData,
  confirmInput,
  PromptUser(..),
  runQnA,
  new,
  newNamed,
  ) where

import           Cli.Qna
import           Cli.ValidInput
import           Cli.YesNo
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Factory as F
import           Recipe

askData :: ValidInput b => Text.Text -> Text.Text -> MaybeT QnA b
askData q n = do
  v <- liftIO $ do
    Text.IO.putStrLn $ q <> ":  "
    Text.IO.getLine
  lift $ recordInput (UserInputRecording n v)
  MaybeT $ return $ parseUserInput v

confirmInput :: QnA a -> MaybeT IO (QnA a)
confirmInput r = return $ do
    ui <- listenInput r
    liftIO $ do
      Text.IO.putStrLn $ formatHistory $ vna ui
      yn <- askYesOrNo "Is this correct? (y/n)"
      case yn of
        No  -> empty
        Yes -> return $! value ui

runQnA :: QnA a -> MaybeT IO a
runQnA qna = liftIO . getAnswers =<< confirmInput qna

formatHistory :: [UserInputRecording] -> Text.Text
formatHistory = foldr c ""
  where c (UserInputRecording name answer) acc =
          name <> ":  " <> answer <> "\n" <> acc

class PromptUser a where
  promptData :: MaybeT QnA a
  promptNamedData :: Text.Text -> MaybeT QnA a

instance PromptUser Recipe where
  promptData = askData "Enter exact item name" "Name" >>= promptNamedData

  promptNamedData name' = do
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

instance PromptUser F.Factory where
  promptData =
    askData "Enter exact factory name (e.g. Constructor)" "Name" >>=
    promptNamedData
  promptNamedData name' = do
    factoryDimensions' <- promptData
    materialsRequired' <- promptData
    inputs' <- askData "Enter amount of inputs (i.e. conveyor belt holes going in)" "Inputs"
    outputs' <- askData "Enter amount of outputs (i.e. conveyor belt holes going out)" "Outputs"
    return (F.Factory name' factoryDimensions' materialsRequired' inputs' outputs')

instance PromptUser F.Dimensions where
  promptData = do
    width' <- askData "Enter width" "Width"
    length' <- askData "Enter length" "Length"
    height' <- askData "Enter height" "Height"
    return $ F.Dimensions width' length' height'
  promptNamedData _ = promptData

instance PromptUser F.MaterialsNeeded where
  promptNamedData _ = promptData
  promptData = F.MaterialsNeeded <$> go Map.empty
    where go m = do
            name' <- askData "Enter name of new material needed" "Material Name"
            amount' <- askData "Enter amount needed of material" "Amount"
            let newMap = Map.insert name' amount' m
            yn <- liftIO $ askYesOrNo "Do you want to add another material? (y/n)"
            case yn of
              No  -> return newMap
              Yes -> go newMap

new :: PromptUser a => MaybeT IO a
new = do
  t <- runQnA $ runMaybeT promptData
  MaybeT $ return t

newNamed :: PromptUser a => Text.Text -> MaybeT IO a
newNamed name = do
  t <- runQnA $ runMaybeT (promptNamedData name)
  MaybeT $ return t

getMaterialsNeeded :: Map.HashMap Text.Text Double -> MaybeT QnA (Map.HashMap Text.Text Double)
getMaterialsNeeded m = do
  name' <- askData "Enter name of new material needed" "Material Name"
  ipm' <- askData "Enter input per minute requirement for the material" "Input per Minute"
  let map'= Map.insert name' ipm' m
  yn <- liftIO $ askYesOrNo "Do you want to add another material? (y/n)"
  case yn of
    No  -> return map'
    Yes -> getMaterialsNeeded map'
