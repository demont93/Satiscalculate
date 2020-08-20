module Cli.Qna
  ( QnA
  , getQnA
  , getAnswers
  , getQuestions
  , recordInput
  , listenInput
  , ResultWithInputHistory
  , vna
  , value
  , UserInputRecording(..)
  ) where

import           Control.Monad.IO.Class
import qualified Data.Text as Text

newtype QnA b = QnA { getQnA :: IO (ResultWithInputHistory b) }

type InputHistory = [UserInputRecording]

data ResultWithInputHistory a
  = ResultWithInputHistory
  { vna   :: InputHistory
  , value :: a
  }
  deriving (Show)

data UserInputRecording
  = UserInputRecording
  { valName :: Text.Text
  , answer  :: Text.Text
  }
  deriving (Show)

instance MonadIO QnA where
  liftIO a = QnA $ ResultWithInputHistory [] <$> a

instance Functor QnA where
  fmap f r = QnA $ do
    (ResultWithInputHistory qa v) <- getQnA r
    return $ ResultWithInputHistory qa (f v)

instance Applicative QnA where
  f <*> r = QnA $ do
    (ResultWithInputHistory qa f')  <- getQnA f
    (ResultWithInputHistory qa' v') <- getQnA r
    return $ ResultWithInputHistory (qa <> qa') $ f' v'

  pure v = QnA $ return $ ResultWithInputHistory [] v


instance Monad QnA where
  r >>= f = QnA $ do
    (ResultWithInputHistory qa v) <- getQnA r
    (ResultWithInputHistory qa' v') <- getQnA $ f v
    return $ ResultWithInputHistory (qa <> qa') v'
  return = pure

recordInput :: UserInputRecording -> QnA ()
recordInput i = QnA $ return $ ResultWithInputHistory [i] ()

listenInput :: QnA a -> QnA (ResultWithInputHistory a)
listenInput m = do
  ui <- liftIO $ getQnA m
  QnA $! return ui{ value = ui }

getAnswers :: QnA a -> IO a
getAnswers qna = do
  (ResultWithInputHistory _ as) <- getQnA qna
  return as

getQuestions :: QnA a -> IO [UserInputRecording]
getQuestions qna = do
  (ResultWithInputHistory qs _) <- getQnA qna
  return qs
