{-# LANGUAGE PackageImports #-}

module RecipeTree
  ( RecipeTree
  , fromDb
  , rawMaterials
  , formatRawMaterials
  , factoriesOrd
  , factoriesTree
  ) where

import             Control.Monad (join)
import "semialign" Data.Align
import             Data.Foldable (foldl')
import qualified   Data.HashMap.Strict as Map
import qualified   Data.HashMap.Strict.InsOrd as OMap
import qualified   Data.Sequence as Seq
import qualified   Data.Text as Text
import qualified   Data.Tree as Tree
import             Db
import             GHC.Generics
import             Recipe

data RecipeLabel = RecipeLabel Recipe Double
                 | MaterialLabel Text.Text Double
                 deriving (Show, Generic)

type RecipeTree = Tree.Tree RecipeLabel

fromDb :: Db -> Text.Text -> Maybe RecipeTree
fromDb db n = do
  r <- recipeFromKey db n
  return $ fromDb' db r (opm r)

fromDb' :: Db -> Recipe -> Double -> RecipeTree
fromDb' db r !n = Tree.Node (RecipeLabel r n) forest
  where
    forest = Map.foldMapWithKey worker mats
    worker :: Text.Text -> Double -> [RecipeTree]
    worker k v = case recipeFromKey db k of
      Just x  -> return $! fromDb' db x (n / opm r * v)
      Nothing -> return $ Tree.Node (MaterialLabel k (n / opm r * v)) []
    mats = materialsNeeded r

rawMaterials :: RecipeTree -> Map.HashMap Text.Text Double
rawMaterials = foldl' step Map.empty
  where step m (MaterialLabel name_ amount_) = Map.insertWith (+) name_ amount_ m
        step m _ = m

formatRawMaterials :: Map.HashMap Text.Text Double -> Text.Text
formatRawMaterials = Map.foldMapWithKey c
  where c k v = k <> ": " <> Text.pack (show v) <> "\n"

factoriesOrd :: RecipeTree -> OMap.InsOrdHashMap Text.Text Int
factoriesOrd tree =
  ceiling <$> foldl' (flip (uncurry (OMap.insertWith (+)))) OMap.empty ordered
  where ordered = Seq.filter ((/= 0) . snd) . postPad $ worker <$> tree
        worker (RecipeLabel Recipe{..} n) = (name, n / opm)
        worker (MaterialLabel n _)        = (n, 0)

postPad :: Tree.Tree a -> Seq.Seq a
postPad t = join $! postPad' t

postPad' :: Tree.Tree a -> Seq.Seq (Seq.Seq a)
postPad' (Tree.Node l xs) = seqs Seq.|> Seq.singleton l
  where
    seqs = foldr (\cur acc -> salign (postPad' cur) acc) Seq.empty xs

factoriesTree :: RecipeTree -> Text.Text
factoriesTree t = Text.pack $ Tree.drawTree (Text.unpack . formatFactory <$> t)

formatFactory :: RecipeLabel -> Text.Text
formatFactory (MaterialLabel name qty) =
  "<<===== " <> name <> " input of " <> Text.pack (show qty)
formatFactory (RecipeLabel Recipe{..} outputNeeded) =
  name <> " " <> factoryName <> ":"
  <> " Qty: " <> Text.pack (show qty)
  <> " Output: " <> Text.pack (show outputNeeded)
  where qty = ceiling $ outputNeeded / opm :: Int
