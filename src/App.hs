module App (
  App,
  runApp,
  Env,
  argus,
  envLogAction,
  verbosity,
  makeEnv
  ) where

import           Cli.Parser
import           Colog
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as S8
import           Data.Default
import           Data.Foldable (foldl')
import qualified Data.Text as Text
import           GHC.Stack

newtype App a = App { unApp :: ReaderT (Env App) IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env App))

runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env

data Env m = Env { envLogAction :: LogAction m Message
                 , argus        :: Params
                 , verbosity    :: Int
                 }

instance MonadIO m => Default (Env m) where
  def = Env richMessageAction (def :: Params) 0

instance Monad m => HasLog (Env m) Message m where
  getLogAction :: Env m -> LogAction m Message
  getLogAction = envLogAction
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction m Message -> Env m -> Env m
  setLogAction newLogAction env = env { envLogAction = newLogAction  }
  {-# INLINE setLogAction #-}

makeEnv :: Args -> PureLogger (Msg Severity) (Env App)
makeEnv args' = setEnvOpts (opts args') env'
  where env' = Env { envLogAction = richMessageAction
                   , argus        = args args'
                   , verbosity    = 1
                   }

setEnvOpts :: [S8.ByteString] -> Env App -> PureLogger (Msg Severity) (Env App)
setEnvOpts xs env = foldl' c (return env) xs
  where c k x = do
          k' <- k
          case x of
                  "-v" -> return $! k'{ verbosity = 2 }
                  _nr  -> do
                    logMessagePure <& Msg W callStack ("Option \"" <> Text.pack (show _nr) <> "\" not found")
                    return k'
