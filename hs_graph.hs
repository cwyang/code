{-# LANGUAGE RankNTypes #-}
import Data.Tree
import Data.Graph
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.Array
import Control.Monad.Trans.Class
import Control.Monad.ST
import Control.Monad.Trans.State
import qualified Data.IntSet as IS
import qualified Data.Sequence as SQ

data Grph node key = Grph
  { _graph :: Graph
  , _vertices :: Vertex -> (node, key, [key])
                                 }

fromList :: Ord key => [(node, key, [key])] -> Grph node key
fromList = uncurry Grph . graphFromEdges'

vertexLabels :: Functor f => Grph b t -> (f Vertex) -> f b
vertexLabels g = fmap (vertexLabel g)

vertexLabel :: Grph b t -> Vertex -> b
vertexLabel g = (\(vi, _, _) -> vi) . (_vertices g)

-- Topologically sort graph
topo' :: Grph node key -> [node]
topo' g = vertexLabels g $ topSort (_graph g)

-- Strongly connected components of graph
scc' :: Grph node key -> [[node]]
scc' g = fmap (vertexLabels g . flatten) $ scc (_graph g)

ex1 :: [(String, String, [String])]
ex1 = [
  ("apple","a",["b"]),
  ("bravo","b",["c"]),
  ("charley","c",["a"])
  ]

ts1 :: [String]
ts1 = topo' (fromList ex1)
-- ["a","b","c"]

sc1 :: [[String]]
sc1 = scc' (fromList ex1)
-- [["a","b","c"]]

ex2 :: [(String, String, [String])]
ex2 = [
  ("a","a",["b"]),
  ("b","b",["c"]),
  ("c","c",["a"]),
  
  ("d","d",["e"]),
  ("e","e",["f", "e"]),
  ("f","f",["d", "e"])
  ]


ts2 :: [String]
ts2 = topo' (fromList ex2)
-- ["d","e","f","a","b","c"]

sc2 :: [[String]]
sc2 = scc' (fromList ex2)
-- [["d","e","f"],["a","b","c"]]
--------------------------------------------------------------------
-- playing with Data.Graph

bfs_try :: Graph -> Vertex -> Tree Vertex
bfs_try g v = head $ run (bounds g) $ chop [generate g v]
  where generate    :: Graph -> Vertex -> Tree Vertex
        generate g v = Node v (map (generate g) (g!v))

chop :: Forest Vertex -> STArray s Vertex Bool -> ST s (Forest Vertex)
chop [] _ = return []
chop (Node v ts : us) m = do
  visited <- contains v m
  if visited then
    chop us m
   else do
    include v m
    as <- chop ts m 
    bs <- chop us m
    return $ Node v as : bs
  where include  :: Vertex -> STArray s Vertex Bool -> ST s ()
        include v m = writeArray m v True
        contains  :: Vertex -> STArray s Vertex Bool -> ST s Bool
        contains v m = readArray m v

run         :: Bounds
               -> (forall s. STArray s Vertex Bool -> ST s (Forest Vertex))
               -> Forest Vertex
run bnds act = runST (newArray bnds False >>= act)
{- Dunno about ST monad and StateT
bfs' :: Graph -> Vertex -> Tree Vertex
bfs' g v = head $ run (bounds g) $ chop' [generate g v]
  where generate    :: Graph -> Vertex -> Tree Vertex
        generate g v = Node v (map (generate g) (g!v))
        
chop' :: Forest Vertex -> MyState s (Forest Vertex)
chop' [] = return []
chop' (Node v ts : us) = do
  visited <- contains v
  if visited then
    chop' us
   else do
    include v
    as <- chop' ts
    bs <- chop' us
    return $ Node v as : bs
  where include  :: Vertex -> MyState s () -- StateT (STArray s Vertex Bool) (ST s) ()
        include v = do
          m <- get
          lift $ writeArray m v True
          put m
        contains  :: Vertex -> MyState s Bool -- StateT (STArray s Vertex Bool) (ST s) Bool
        contains v = do
          m <- get
          v <- lift $ readArray m v
          put m
          return v

run'         :: Bounds -> MyState s (Forest Vertex) -> Forest Vertex
run' bnds act = runST $ do
  s <- newArray bnds False
  evalStateT act s

type MyState s = StateT (STArray s Vertex Bool) (ST s)
-}

-- Jeremmy Gibbons Breadth-First Traversal

levels' :: Tree a -> [[a]]
levels' (Node v ts) = [v] : foldr (lzw (++)) [] (map levels' ts)

lzw :: (a->a->a) -> [a] -> [a] -> [a]  -- long zip with
lzw f (a:x) (b:y) = f a b : lzw f x y
lzw f x [] = x
lzw f [] y = y

-- stackoverflow alternative's bfs

bfs :: Graph -> Vertex -> [[Vertex]]
bfs graph start = go IS.empty graph $ SQ.singleton [start]
  where go :: IS.IntSet -> Graph -> SQ.Seq [Vertex] -> [[Vertex]]
        go seen graph queue =
          case SQ.viewl queue of
            SQ.EmptyL -> []
            vs SQ.:< rest -> vs : go seen'' graph queue'
              where seen' = foldr IS.insert seen vs
                    (neighbors,seen'') = foldr gogo ([],seen') vs
                    queue' | null neighbors = rest 
                           | otherwise      = rest SQ.>< SQ.singleton neighbors
                    gogo :: Vertex -> ([Vertex], IS.IntSet) -> ([Vertex], IS.IntSet)
                    gogo v (n,s) = let ns = filter (not . flip IS.member s) $ graph ! v
                                       s' = foldr IS.insert s ns
                                   in (n ++ ns, s')

g1 :: [Edge]
g1 = [
  (0,1),
  (1,2),
  (1,3),
  (3,4)]
undirFilter :: [Edge] -> [Edge]
undirFilter = concatMap (\(x,y) -> [(x,y), (y,x)])
test = bfs (buildG (0,4) $ undirFilter g1) 0


