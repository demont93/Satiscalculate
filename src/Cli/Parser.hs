module Cli.Parser (
  -- * Data
  Args,
  -- * Accesors
  args,
  opts,
  -- * Bundled patterns
  pattern Command,
  -- * Functions
  parseArgs,
  Params,
  ) where

import qualified Data.ByteString.Char8 as S8
import           Data.Vector ((!?))
import qualified Data.Vector as V
import           Data.Default

type StringVec = V.Vector S8.ByteString

data Args = Args { args :: Params
                 , opts :: [S8.ByteString]
                 }

newtype GParams a = GParams (V.Vector a)
                  deriving newtype (Functor)

instance Default (GParams a) where
  def = GParams V.empty

type Params = GParams S8.ByteString

pattern Command :: S8.ByteString -> StringVec -> Params

pattern Command command args <- (getCommand -> (Just command, args))

parseArgs :: [String] -> Args
parseArgs xs = Args (GParams $ V.fromList $ S8.pack <$> args) $ S8.pack <$> opts
  where (args, rest) = span isArg xs
        opts = takeWhile isOpt rest

isArg :: String -> Bool
isArg x = take 1 x /= "-"

isOpt :: String -> Bool
isOpt x = take 1 x == "-"

getCommand :: Params -> (Maybe S8.ByteString, StringVec)
getCommand (GParams v) = (v !? 0, V.drop 1 v)
