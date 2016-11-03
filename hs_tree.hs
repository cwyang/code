import qualified Data.IntMap as Map
import Data.IntMap (IntMap)
import Data.Array

-- Rose Tree
data Tree = Node { v :: Int
                 , depth :: Int
                 , children :: IntMap Tree
                 } deriving Show

mkTree :: Int -> [Int] -> [(Int,Int)] -> Array Int Tree
mkTree n vs edges = treeArr
  where varr = listArray (1,n) vs
        edgeArr :: Array Int [Int]
        edgeArr = accumArray (\x y -> y:x) [] (1,n) $
                  concatMap (\(x,y) -> [(x,y),(y,x)]) edges
        treeArr :: Array Int Tree
        treeArr = array (1,n) [ (k, go k) | k <- [1..n] ]
        go 1 = Node { v = varr!1
                    , depth = 0
                    , children = Map.fromList $
                      map (\x -> (x,treeArr!x)) (edgeArr!1)}
        go k = Node { v = varr!k
                    , depth = 0
                    , children = Map.fromList $
                      map (\x -> (x,treeArr!x)) (edgeArr!1)}
