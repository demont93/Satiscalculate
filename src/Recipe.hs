module Recipe where


import           Data.Aeson
import Data.Aeson.Types (Parser())
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import           GHC.Generics


data Recipe = Recipe { name :: Text.Text
                     , opm  :: Double
                     , craftingSeconds :: Double
                     , materialsNeeded :: Map.HashMap Text.Text Double
                     , factoryName :: Text.Text
                     } deriving (Show, Generic)


instance ToJSON Recipe where
  toJSON Recipe{..} = object
    [ "name" .= name
    , "opm" .= opm
    , "craftingSeconds" .= craftingSeconds
    , "materialsNeeded" .= mats
    , "factoryName"     .= factoryName
    ]
    where mats = V.fromList $
            Map.foldrWithKey
            (\k v acc -> object ["name" .= k, "ipm".= v] : acc)
            []
            materialsNeeded


instance FromJSON Recipe where
  parseJSON = withObject "recipe" $ \v -> do
    name_ <- v .: "name"
    opm_ <- v .: "opm"
    craftingSeconds_ <- v .: "craftingSeconds"
    materialsNeeded_ <- parseMaterials =<< v .: "materialsNeeded"
    factoryName_ <- v .: "factoryName"
    return $ Recipe name_ opm_ craftingSeconds_ materialsNeeded_ factoryName_


parseMaterials :: Value -> Parser (Map.HashMap Text.Text Double)
parseMaterials =
  withArray "material list" $ \a -> V.foldM step Map.empty a
  where step acc = withObject "one material" $ \o -> do
          ipm_ <- o .: "ipm"
          name_ <- o .: "name"
          return $ Map.insert name_ ipm_ acc
