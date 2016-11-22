-- UnionFind by kyseo
import Data.List
import Data.Ord
import Data.Char
import Data.Function
import Data.Bits
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Data.Array.IO
import Data.Array((!), listArray, Array, elems, accumArray)
import Data.Array.Unboxed(UArray)
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Modifiers (NonEmptyList (..))
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

-- Brent's Cycle Finding

brent :: Eq a => [a] -> (Int, Int)
brent xs = (lambda, mu)
  where lambda = findLambda 1 1 (head xs) (tail xs)
        mu     = let hare = drop lambda xs
                 in length . takeWhile (uncurry (/=)) $ zip xs hare
        findLambda :: Eq a => Int -> Int -> a -> [a] -> Int
        findLambda power lambda tortoise (hare:xs)
          | tortoise == hare      = lambda
          | power == lambda       -- time to start a new power of two
            = findLambda (power*2) 1 hare xs
          | otherwise             = findLambda power (lambda+1) tortoise xs

brentTest = do
  quickCheck (test1)
  where 
    test1 :: (Positive Int) -> (Positive Int) -> Bool
    test1 (Positive l) (Positive m) = brent ([1..m] ++ cycle [m+1..l+m]) == (l,m)

-- Suffix Array

-- | O(N^2*log N)
makeNaiveSA :: Ord a => [a] -> [Int] -- Array Int (Int,[a])
makeNaiveSA xs = tail . map fst {-. listArray (0,n)-} . sortBy (comparing snd) . zip [0..] . tails $ xs
  where n = length xs
-- | O(N * (log N)^2) try -- CMU 15-210 Lecture 25 (Spring 2012)
makeSA :: Ord a => [a] -> [Int] 
makeSA xs = map fst . sortBy (comparing snd) $ zip [0..] v
  where len = length xs
        v = fst $ rank xs len len
rank :: Ord a => [a] -> Int -> Int -> ([Int],[Int])
rank xs 1 len = rankBase xs len
rank xs l len = let half_l = myHalf l
                    (a,b) = rank xs half_l len 
                    pairs = zip [0..] $ zip a b
                    sortedPairs = sortBy (comparing snd) pairs
{-
                    sortedPairs = countingSortBy (-1,len) (\(_,(x,_)) -> x)
                                . countingSortBy (-1,len) (\(_,(_,x)) -> x) $ pairs
-}
                    newRank = transform sortedPairs
                    myHalf n = last . takeWhile (\x -> x < n)  $ iterate (*2) 1
                in (take len newRank, take len $ drop l newRank)

rankBase :: Ord a => [a] -> Int -> ([Int],[Int])
rankBase xs len = (take len newRank, take len $ tail newRank)
  where sortedPairs  = sortBy (comparing snd) $ zip [0..] xs
        newRank = transform sortedPairs
        
transform :: Eq a => [(Int,a)] -> [Int]
transform xs = (map snd . sortBy (comparing fst) $ v) ++ repeat (-1)
  where grouped = groupBy ((==) `on` snd) xs
        v = concat $ zipWith (\xs k -> map (\x -> (fst x,k)) xs) grouped [0..]

countingSortBy :: (Int,Int) -> (a->Int) -> [a] -> [a]
countingSortBy bnd fn xs = concatMap reverse $ elems arr
  where arr = accumArray (flip (:)) [] bnd $ zip (map fn xs) xs

testS = last $ countingSortBy (1,maxv) (id) a
  where maxv = 50000000
        a = [maxv,maxv-1..1]
testS1 = last $ sort a
  where maxv = 50000000
        a = [maxv,maxv-1..1]

testSA = length $ makeNaiveSA "GATAGACA$"
testSA1 = length $ makeNaiveSA $ replicate 20000 'G' ++ "GATAGACA$"                            
testSA2 = length $ makeSA "GATAGACA$"
testSA3 = length $ makeSA $ replicate 200000 'G' ++ "GATAGACA$"                            
stringMatch :: String -> String -> Maybe [Int]
stringMatch src pattern
  | takeVal (sa V.! low)  /= pattern = Nothing
  | takeVal (sa V.! high) /= pattern = Just [ sa V.! x | x <- [low..high-1] ]
  | otherwise               = Just [ sa V.! x | x <- [low..high] ]
  where n = length src
        m = length pattern
        srcVector = V.fromList src
        sa = V.fromList $ makeSA src
        takeVal v = V.toList . V.take m $ V.drop v srcVector
        low = binarySearch 0 (n-1) (<=)
        high = binarySearch 0 (n-1) (<)
        binarySearch :: Int -> Int -> (String->String->Bool) -> Int
        binarySearch l h cmp -- make narrow on l
          | l == h          = l
          | pattern `cmp` p = binarySearch l mid cmp
          | otherwise       = binarySearch (mid+1) h cmp
          where mid = (l + h) `div` 2
                p = takeVal (sa V.! mid)

stringMatchTest = stringMatch "parallel" "al"
          
