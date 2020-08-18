module Cli.ValidInput
  ( ValidInput (..)
  ) where


import qualified Data.Text as Text
import qualified Data.Text.Read as Text.R



class ValidInput a where
  parseUserInput :: Text.Text -> Maybe a


instance ValidInput Int where
  parseUserInput v = case Text.R.decimal v of
                      Left _        -> Nothing
                      Right (v', _) -> Just v'


instance ValidInput Double where
  parseUserInput v = case Text.R.double v of
                      Left _        -> Nothing
                      Right (v', _) -> Just v'


instance ValidInput Text.Text where
  parseUserInput s = if s == "" then Nothing else Just s
