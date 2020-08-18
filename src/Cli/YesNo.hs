module Cli.YesNo
  ( YN (Yes, No)
  , askYesOrNo        -- Constructor
  ) where


import qualified Data.Text    as Text
import qualified Data.Text.IO as Text.IO


type Question = Text.Text




data YN = Yes | No deriving (Show)

-- Constructor
askYesOrNo :: Question -> IO YN
askYesOrNo s = do
  Text.IO.putStrLn s
  yn <- Text.IO.getLine
  Text.IO.putStrLn ""
  case yn of
    "y" -> return Yes
    _   -> return No
