module Cli.Qna
  ( QnA
  , getQnA
  , getAnswers
  , getQuestions
  , recordInput
  , listenInput
  , UserInput
  , vna
  , value
  , UserInteraction(..)
  ) where


import           Control.Monad.IO.Class
import qualified Data.Text as Text

newtype QnA b = QnA { getQnA :: IO (UserInput b) }


data UserInput a = UserInput { vna   :: [UserInteraction]
                             , value :: a
                             } deriving (Show)


data UserInteraction = UserInteraction { valName :: Text.Text
                                       , answer  :: Text.Text
                                       } deriving (Show)


instance MonadIO QnA where
  liftIO a = QnA $ UserInput [] <$> a


instance Functor QnA where
  fmap f r = QnA $ do
    (UserInput qa v) <- getQnA r
    return $ UserInput qa (f v)


instance Applicative QnA where
  f <*> r = QnA $ do
    (UserInput qa f')  <- getQnA f
    (UserInput qa' v') <- getQnA r
    return $ UserInput (qa <> qa') $ f' v'

  pure v = QnA $ return $ UserInput [] v


instance Monad QnA where
  r >>= f = QnA $ do
    (UserInput qa v) <- getQnA r
    (UserInput qa' v') <- getQnA $ f v
    return $ UserInput (qa <> qa') v'
  return = pure


recordInput :: UserInteraction -> QnA ()
recordInput i = QnA $ return $ UserInput [i] ()


listenInput :: QnA a -> QnA (UserInput a)
listenInput m = do
  ui <- liftIO $ getQnA m
  QnA $! return ui{ value = ui }


getAnswers :: QnA a -> IO a
getAnswers qna = do
  (UserInput _ as) <- getQnA qna
  return as


getQuestions :: QnA a -> IO [UserInteraction]
getQuestions qna = do
  (UserInput qs _) <- getQnA qna
  return qs

