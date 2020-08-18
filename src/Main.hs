module Main where

import           App
import           Cli.Parser
import           Colog
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import qualified Data.Text as Text
import           Data.Text.Encoding
import qualified Data.Text.IO as Text.IO
import           Data.Typeable
import           Data.Vector ((!))
import           Db
import           Db.Utils
import           Prelude hiding (log)
import           RecipeTree
import           System.Environment

newtype DbException = DbException String
                 deriving (Typeable, Show)

instance Exception DbException
main :: IO ()
main = do
  allArgs <- parseArgs <$> getArgs
  let (env, msgs) = runPureLog $ makeEnv allArgs
  mapM_ (unLogAction richMessageAction) msgs
  runApp env app

app :: App ()
app = do
  args <- asks argus
  eitherDb <- liftIO $ loadDb "db/db.json"
  logl 2 I "Loading db from db/db.json"
  db <- case eitherDb of
    Left e  -> liftIO $ throwIO $ DbException ("Error reading db.\n" ++ show e)
    Right v -> return v
  getCommand args db
  liftIO $ Text.IO.putStrLn "Bye!"

------------------------------------------------------------------------------
------------------------------------------------------------------------------

getCommand :: Params -> Db -> App ()
getCommand (Command "db" _) db   = void $ liftIO $ addManyRecipesToDbWithUserInput db
getCommand (Command "raw" args) db =
  liftIO $ Text.IO.putStrLn $ case fromDb db item of
               Just x ->
                 " -- Raw Materials -- \n"
                 <> item
                 <> ":\n"
                 <> formatRawMaterials (rawMaterials x)
               Nothing ->
                 "Item not found"
  where item = decodeUtf8 $ args ! 0
getCommand (Command "factories" args) db =
  liftIO $ Text.IO.putStrLn $ case fromDb db item of
                                Just x  ->
                                  " -- Factory Tree -- \n"
                                  <> factoriesTree x
                                Nothing ->
                                  "Item not found"
  where item = decodeUtf8 $ args ! 0
getCommand _ _ = liftIO $ Text.IO.putStrLn "No command selected..."

logl :: (MonadReader (Env m) m)
     => Int
     -> Severity
     -> Text.Text
     -> m ()
logl n sev s = do
  level <- asks verbosity
  when (level == n) $ log sev s
