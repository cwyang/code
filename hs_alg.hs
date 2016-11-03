-- UnionFind by kyseo
import Data.Bits
import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Data.Array.IO
import Data.Array((!), listArray, Array)
import Data.Array.Unboxed(UArray)
import Debug.Trace

data UnionFind s = UnionFind { ids:: STUArray s Int Int
                             , szs:: STUArray s Int Int
                             }
newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftA2 UnionFind (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)
find :: (UnionFind s) -> Int -> Int -> ST s Bool
find uf p q = liftA2 (==) (root uf p) (root uf q)
root :: (UnionFind s) -> Int -> ST s Int
root uf i = do
  id <- readArray (ids uf) i
  if (id /= i)
    then do gpid <- readArray (ids uf) id
            writeArray (ids uf) i gpid
            root uf id
    else return i
unite :: (UnionFind s) -> Int -> Int -> ST s ()
unite uf p q = do
  i <- root uf p
  j <- root uf q
  when (i /= j) $ do
    szi <- readArray (szs uf) i
    szj <- readArray (szs uf) j
    if (szi < szj)
      then do writeArray (ids uf) i j
              writeArray (szs uf) j (szi + szj)
      else do writeArray (ids uf) j i
              writeArray (szs uf) i (szj + szi)

-- sizes of corresponding elements, for hackerrank
getSize :: (UnionFind s) -> ST s [Int]
getSize uf = do
  a <- getAssocs . ids $ uf
  b <- getElems . szs $ uf
  return . map snd . filter (\((k,v),_) -> k == v) $ zip a b

run = print $ runST $ do
  uf <- newUnionFind 10
  unite uf 3 4 -- 0, 1, 2, {3, 4}, 5, 6, 7, 8, 9
  unite uf 4 9 -- 0, 1, 2, {3, 4, 9}, 5, 6, 7, 8
  unite uf 8 0 -- {0, 8}, 1, 2, {3, 4, 9}, 5, 6, 7, 8
  unite uf 2 3 -- {0, 8}, 1, {2, 3, 4, 9}, 5, 6, 7
  unite uf 5 6 -- {0, 8}, 1, {2, 3, 4, 9}, {5, 6}, 7
  unite uf 5 9 -- {0, 8}, 1, {2, 3, 4, 5, 6, 9}, 7
  unite uf 7 3 -- {0, 8}, 1, {2, 3, 4, 5, 6, 7, 9}
--  find uf 1 2 -- False
  getSize uf

-- Segment Tree.. ugly :-(
data SegmentTree s a = SegmentTree { size :: Int
                                   , op :: a->a->a
                                   , seg  :: STArray s Int (Maybe a)
                                   }
left, right:: Int -> Int
left x = x*2
right x = x*2+1
half :: Int -> Int -> Int
half a b = (a+b) `div` 2
newSegmentTree :: Int -> [a] -> (a->a->a) -> ST s (SegmentTree s a)
newSegmentTree n xs op = do
  SegmentTree n' op <$> newListArray (0,2*n'-1) (replicate n' Nothing ++ map Just xs ++ repeat Nothing)
  where n' = 2 ^ (ceiling (logBase 2 (fromIntegral n)))
buildSegmentTree :: (Show a, Ord a) => Int -> [a] -> (a->a->a) -> ST s (SegmentTree s a)
buildSegmentTree n xs func = do
  st <- newSegmentTree n xs func
  build' st 1 0 (size st)
  return st
  where build' :: (Ord a) => SegmentTree s a -> Int -> Int -> Int -> ST s ()
        build' st p l r
--          | trace (show (p,l,r)) False = undefined
          | l == r-1 = return ()
          | otherwise = do
              build' st (left p)  l (half l r)
              build' st (right p) (half l r) r
              v1 <- readArray (seg st) (left p)
              v2 <- readArray (seg st) (right p)
              let nv = case (v1,v2) of
                    (Nothing,_) -> v2
                    (_,Nothing) -> v1
                    otherwise   -> liftA2 (op st) v1 v2
              writeArray (seg st) p nv
updateSegmentTree :: (Ord a) => SegmentTree s a -> Int -> a -> ST s ()
updateSegmentTree st k v = do
  writeArray (seg st) pos (Just v)
  goUp st (pos `div` 2)
  where pos = (size st) + k
        goUp :: SegmentTree s a -> Int -> ST s()
        goUp st 0 = return ()
        goUp st p = do
          v1 <- readArray (seg st) (left p)
          v2 <- readArray (seg st) (right p)
          writeArray (seg st) p $ liftA2 (op st) v1 v2
          goUp st (p `div` 2)
querySegmentTree st i j = go st 1 0 (size st) i j
  where go :: (Ord a) => SegmentTree s a -> Int -> Int -> Int -> Int -> Int -> ST s (Maybe a)
        go st p l r i j
--          | trace (show (p,l,r,i,j)) False = undefined
          | i >= r || j <= l = return Nothing
          | l == i && r == j = readArray (seg st) p
          | m < i            = go st (right p) m r i j
          | j <= m           = go st (left p)  l m i j
          | otherwise = do
              v1 <- go st (left p) l m i m
              v2 <- go st (right p) m r m j
              case (v1,v2) of
                (Nothing,_) -> return v2
                (_,Nothing) -> return v1
                otherwise   -> return $ liftA2 (op st) v1 v2
                where m = half l r
runSeg,runSeg2 :: IO ()
runSeg = do
  let r = runST $ do
        st <- buildSegmentTree 9 (map (replicate 2) [10,2,47,3,7,9,1,98,21])
          (\[a,b] [c,d] -> [min a c,max b d])  -- (min,max)
        let query = [(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9)]
        r1 <- mapM (\(x,y) -> querySegmentTree st x y) query
        t1 <- getElems (seg st)
        updateSegmentTree st 1 [8,8]
        r2 <- mapM (\(x,y) -> querySegmentTree st x y) query
        t2 <- getElems (seg st)
        return (r1,r2)
--        return (t1, t2)
  print $ show r
runSeg2 = do
  let a = sequence $ runST $ do
        st <- buildSegmentTree 100000 [1..100000] (\x y -> x + y)
        let query = [(0,1),(0,2),(10,11),(0,4),(0,5),(0,6),(0,7),(0,8),(0,100000)]
        mapM (\(x,y) -> querySegmentTree st x y) query
  print $ show a
              
-- Fenwick Tree -- 1 based index
data FenwickTree s = FenwickTree { sizeFT :: Int
                                 , fen  :: IOArray Int Int
                                 }
lsOne :: Int -> Int
lsOne x = x .&. (-x)
newFenwickTree :: Int -> [Int] -> IO (FenwickTree s)
newFenwickTree n xs = do
  ft <- FenwickTree n <$> newArray (0,n) 0
  mapM_ (uncurry $ updateFenwickTree ft) $ zip [1..] xs
  return ft
updateFenwickTree :: FenwickTree s -> Int -> Int -> IO ()
updateFenwickTree ft p delta
  | p > (sizeFT ft) = return ()
  | otherwise       = do
      writeArray (fen ft) p . (+ delta) =<< readArray (fen ft) p
      updateFenwickTree ft (p + lsOne p) delta -- clever.
queryFenwickTree :: FenwickTree s -> Int -> Int -> IO Int  -- [s,t] inclusive
queryFenwickTree _ _ end | end < 1 = return 0
queryFenwickTree ft 1 end     = do
  v1 <- readArray (fen ft) end
  v2 <- queryFenwickTree ft 1 (end - lsOne end)
  return (v1 + v2)
queryFenwickTree ft start end = do
  ve <- queryFenwickTree ft 1 end
  vs <- queryFenwickTree ft 1 (start - 1)
  return (ve-vs)
runFen :: IO ()
runFen = do
  let score = [2,4,5,5,6,6,6,7,7,8,9]
  --  ft <- newFenwickTree 10 [2,4,5,5,6,6,6,7,7,8,9]
  ft <- newFenwickTree 10 []
  mapM_ (flip (updateFenwickTree ft) 1) score
  let query = [(1,1), (1,2), (1,6), (1,10), (3,6)]
  r1 <- mapM (uncurry $ queryFenwickTree ft) query
  updateFenwickTree ft 5 2 
  r2 <- queryFenwickTree ft 1 10
  print (r1,r2)

  
