{-# LANGUAGE BangPatterns #-}
import Data.Array
import Data.Array.ST
import Data.List (sort, foldl')
import Control.Monad.ST
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import Control.Monad (when)
import Text.Printf
import Data.List.Split
-- | MultiSet Helper
newtype MultiSet a = MS { unMS :: M.Map a Int }
emptyMS :: MultiSet a
emptyMS = MS M.empty
nullMS  :: MultiSet a -> Bool
nullMS (MS m) = M.null m
insertMS :: Ord a => a -> MultiSet a -> MultiSet a
insertMS v (MS m) = MS $ M.insertWith (+) v 1 m
deleteMS :: Ord a => a -> MultiSet a -> MultiSet a
deleteMS v (MS m) = MS $ M.updateWithKey f v m
  where f k 1 = Nothing
        f k n = Just (n-1)
deleteFindMinMS :: Ord a => MultiSet a -> (a, MultiSet a)
deleteFindMinMS (MS m) = (mv, deleteMS mv (MS m))
  where (mv,_) = M.findMin m

-- | Weighted Graph
type Vertex  = Int
type Graph a = Array Vertex [(Vertex, a)]
type Edge a  = (Vertex, Vertex, a)
type EdgeList a = [(a, (Vertex, Vertex))]
type AdjMatrix a = Array (Vertex,Vertex) a

graphFromEdges' :: [(Vertex, [(Vertex, a)])] -> Graph a
graphFromEdges' !edges = array bounds edges
  where
    bounds = (0, length edges - 1)

graphFromEdges :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdges bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w)), (b, (a,w))]) edges
graphFromEdgesDirected :: (Int, Int) -> [Edge a] -> Graph a
graphFromEdgesDirected bounds edges = accumArray (flip (:)) [] bounds edges'
  where edges' = concatMap (\(a,b,w) -> [(a, (b,w))]) edges

buildAdjMatrix :: (Int, Int) -> [Edge Int] -> AdjMatrix (Maybe Int)
buildAdjMatrix (a,b) edges = accumArray (flip const) Nothing ((a,a),(b,b)) edges'
  where edges' = map (\(a,b,w) -> ((a,b),Just w)) edges

toEdgeList :: [(Vertex,Vertex,a)] -> [(a, (Vertex,Vertex))]
toEdgeList = map (\(a,b,w) -> (w, (a,b)))

-- | All vertices of a graph.
vertices :: Graph a -> [Vertex]
vertices = indices

-- | All edges of a graph.
edges :: Graph a -> [Edge a]
edges g = [ (v, w, a) | v <- vertices g, (w, a) <- g ! v ]

dijkstra :: (Show a, Eq a, Ord a, Num a) => Graph a -> S.Set (a,Vertex) -> M.Map Vertex a -> M.Map Vertex a
dijkstra graph pq dist
  | S.null pq                  = dist
  | d_u' /= Nothing && d_u < d = dijkstra graph pq' dist
  | otherwise                  = dijkstra graph new_pq new_dist
  where ((d,u), pq') = S.deleteFindMin pq
        d_u' = M.lookup u dist
        d_u = case d_u' of {Just v -> v; Nothing -> 0}
        (new_pq, new_dist) = foldr go (pq', dist) [ (x,w) | (x,w) <- graph ! u,
                                                    let d_x' = M.lookup x dist,
                                                    d_x' == Nothing ||
                                                    d_u + w < fromJust d_x' ]
        go (x,w) (pq,dist) = let new_d = d_u + w
                             in (S.insert (new_d,x) pq, M.insert x new_d dist)

dijkstra' :: (Show a, Ord a, Num a) => Graph a -> Vertex -> M.Map Vertex a
dijkstra' graph start = dijkstra graph (S.singleton (0,start)) (M.singleton start 0)

bellmanFord :: (Show a, Ord a, Num a) => Graph a -> Vertex -> M.Map Vertex a
bellmanFord graph start = foldl' relax (M.singleton start 0)
                          [ (u,v,w) | i <- tail r, u <- r, (v,w) <- graph ! u ]
  where r = range $ bounds graph -- relax all E edges V-1 times
        relax m (u,v,w) = case (M.lookup u m, M.lookup v m) of
          (Nothing, _)       -> m
          (Just u', Nothing) -> M.insert v (u'+w) m
          (Just u', Just v') -> M.insert v (min v' (u'+w)) m
negativeCycle :: (Show a, Ord a, Num a) => Graph a -> Vertex -> Bool
negativeCycle graph start = foldr check False [ (u,v,w) | u <- r, (v,w) <- graph ! u ]
  where r = range $ bounds graph
        m = bellmanFord graph start
        check (u,v,w) acc = case (M.lookup u m, M.lookup v m) of
          (Just u', Just v') -> v' > u'+ w || acc
          otherwise          -> error "error"

-- All pair shortest path DP algorithm
floydWarshall :: AdjMatrix (Maybe Int) -> (AdjMatrix (Maybe Int), (Vertex -> Vertex -> [Vertex]))
floydWarshall am = runST $ do
  arr <- thaw am
  parent <- newListArray (bounds am) [ i | (i,j) <- range (bounds am) ]
  sequence_ [ go arr parent k i j | k <- r, i <- r, j <- r]
  a <- freeze arr
  b <- freeze parent
  return (a,(reverse .).shortestPath b)
  where ((minb,_), (maxb,_)) = bounds am
        r = [minb..maxb]
        go :: STArray s (Vertex,Vertex) (Maybe Int)
           -> STArray s (Vertex,Vertex) Int
           -> Vertex -> Vertex -> Vertex -> ST s ()
        go arr parent k i j = do
          a <- readArray arr (i,j)
          b <- (liftA2 (+)) <$> readArray arr (i,k) <*> readArray arr (k,j)
          when (myMin a b == b) $ do
            writeArray arr (i,j) $ myMin a b
            writeArray parent (i,j) =<< readArray parent (k,j)
        myMin Nothing x = x
        myMin x Nothing = x
        myMin x y = min x y
        shortestPath :: Array (Vertex,Vertex) Int -> Vertex -> Vertex -> [Vertex]
        shortestPath arr start end
          | start /= end = end : shortestPath arr start (arr!(start,end))
          | otherwise    = [end]

buildAdjMatrix' :: (Int, Int) -> [Edge Int] -> AdjMatrix Int
buildAdjMatrix' (a,b) edges = accumArray (flip const) big ((a,a),(b,b)) edges'
  where edges' = map (\(a,b,w) -> ((a,b), w)) edges
        big = 10^9

floydWarshall' :: AdjMatrix Int -> AdjMatrix Int
floydWarshall' am = runST $ do
  arr <- thaw am
  sequence_ [ go arr k i j | k <- r, i <- r, j <- r]
  freeze arr
  where ((minb,_), (maxb,_)) = bounds am
        r = [minb..maxb]
        go :: STArray s (Vertex,Vertex) (Int)
           -> Vertex -> Vertex -> Vertex -> ST s ()
        go arr k i j = do
          a <- readArray arr (i,j)
          b <- (+) <$> readArray arr (i,k) <*> readArray arr (k,j)
          writeArray arr (i,j) $ min a b
  
floydWarshallDP :: AdjMatrix (Maybe Int) -> AdjMatrix (Maybe Int)
floydWarshallDP am = go 0 am 
  where ((minb,_), (maxb,_)) = bounds am
        r = [minb..maxb]
        go k am
          | k > maxb = am
          | otherwise = let am' = array (bounds am)
                              [ ((i,j), myMin ij viak)  | (i,j) <- range (bounds am)
                                                        , let ij = am!(i,j)
                                                              viak = (+) <$> am!(i,k) <*> am!(k,j)]
                        in go (k+1) am'
        myMin Nothing x = x
        myMin x Nothing = x
        myMin x y = min x y
        
prim :: Graph Int -> Vertex -> Int
prim graph start = prim' pq taken 0
  where (pq, taken) = process start S.empty S.empty
        process vertex pq taken = (pq', taken')
          where taken' = S.insert vertex taken
                pq'    = foldr S.insert pq [ (w, x) | (x,w) <- graph ! vertex,
                                                        not (S.member x taken) ]
                -- sort by (inc) weight then by (inc) id
        prim' pq taken mst_cost
          | S.null pq         = mst_cost
          | S.member id taken = prim' pq' taken mst_cost
          | otherwise         = let (pq'', taken') = process id pq taken
                                in traceShow id $ prim' pq'' taken' (mst_cost + weight)
          where ((weight, id), pq') = S.deleteFindMin pq

kruskal :: (Int,Int) -> EdgeList Int -> Int
kruskal (s,e) edges = runST $ do
  uf <- newUnionFind (s,e) (e-s+1)
  kruskal' (sort edges) 0 uf
  where kruskal' :: EdgeList Int -> Int -> UnionFind s -> ST s Int
        kruskal' [] cost _ = return cost
        kruskal' ((w,(u,v)):es) cost uf = do
          cycle <- find uf u v
          if cycle then
            kruskal' es cost uf
           else do
--            traceM (show (u,v,w))
            unite uf u v
            kruskal' es (cost+w) uf

data UnionFind s = UnionFind { ids:: STUArray s Int Int, szs:: STUArray s Int Int}
newUnionFind :: (Int,Int) -> Int -> ST s (UnionFind s)
newUnionFind bound n = liftA2 UnionFind (newListArray bound $ range bound) (newArray bound 1)
find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftA2 (==) (root uf p) (root uf q)
root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
  id <- readArray (ids uf) i
  if (id /= i)
    then do {gpid <- readArray (ids uf) id; writeArray (ids uf) i gpid; root uf id}
    else return i
unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do { i <- root uf p; j <- root uf q;
                    szi <- readArray (szs uf) i; szj <- readArray (szs uf) j;
                    if (szi < szj)
                    then do {writeArray (ids uf) i j; writeArray (szs uf) j (szi + szj)}
                    else do {writeArray (ids uf) j i; writeArray (szs uf) i (szj + szi)}}

g1,g2 :: [Edge Int]
g1 = [
  (0,2,6),
  (0,4,1),
  (1,2,2),
  (1,3,3),
  (1,4,6),
  (2,3,-79),
  (3,4,5)]
g2 = [
  (0,1,4),
  (0,2,4),
  (0,3,6),
  (0,4,6),
  (1,2,2),
  (2,3,8),
  (3,4,9)]
g3 = [
  (1,2,9),
  (2,1,-10),
  (2,3,-8),
  (1,3,2)]
g4 = [
  (0,1,2),
  (0,2,1),
  (0,4,3),
  (1,3,4),
  (2,1,1),
  (2,4,1),
  (3,0,1),
  (3,2,3),
  (3,4,5)]
printArray :: (Show a) => Array (Int,Int) a -> String
printArray arr = unlines . map (concatMap $ printf "%10s" . show) . chunksOf row $ elems arr
  where ((x,_),(x',_)) = bounds arr
        row = x'-x+1
test = graphFromEdgesDirected (1,3) g3
test2 = dijkstra' test 1
test2' = bellmanFord test 1
test3 = flip prim 0 $ graphFromEdges (0,4) g2
test4 = kruskal (0,4) $ toEdgeList g2
test5 = buildAdjMatrix (0,4) g4
test5' = floydWarshall test5
test5'' = floydWarshallDP test5
