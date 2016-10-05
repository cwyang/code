import Data.Foldable
import qualified Data.Map as Map

data Elt
  = Elt Int Double
  | Nil

buildMap :: [Elt] -> Map.Map Int Double
buildMap = foldMap go
  where
    go (Elt x y) = Map.singleton x y
    go Nil = Map.empty

test = buildMap $ map (uncurry Elt) $ zip [1..100] [1.0..100.0]
