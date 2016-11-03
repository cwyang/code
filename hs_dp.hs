-- Longest Increasing Subsequence (LIS)
-- Given a sequence {A[0],..,A[n-1]}, determine its Longest Increasing Subsequence.

import System.Random
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List
import Data.Ord
import Criterion.Main
import Debug.Trace
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Data.Bits

mkRandomInts :: Int -> [Int]
mkRandomInts n = go n (mkStdGen 0)
  where go 0 _ = []
        go n gen = let (a,gen') = (random gen)
                   in (abs a `mod` 10001):go (n-1) gen'

isIncreasing :: (Ord a) => [a] -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x:y:xs)
  | x < y = isIncreasing (y:xs)
  | otherwise = False
    
lisBrute, lisBrute', lisDP, lisDC :: [Int] -> [Int]

lisBrute = head . sortBy (comparing (negate . length)) . filter isIncreasing . powerset
lisBrute' = head . sortBy (comparing (negate . length)) . filter isIncreasing . powerset' Nothing
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)
powerset' _ [] = [[]]
powerset' Nothing (x:xs) = powerset' Nothing xs ++ map (x:) (powerset' (Just x) xs)
powerset' (Just p) (x:xs)
  | p < x = powerset' (Just p) xs ++ map (x:) (powerset' (Just x) xs)
  | otherwise = powerset' (Just p) xs

-- lis(i) is the LIS ending at index i
-- Index  0  1  2  3  4  5  6  7
-- A     -1 10  9  2  3  8  8  1
-- lis(i) 1  2  2  2  3  4  4  2
lisLengthDP :: [Int] -> Int
lisLengthDP xs = maximum $ elems lis
  where n = length xs
        xs' = zip [0..] xs
        lis = array (0,n-1) $
          [(0,1)] ++ [ (p, v+1) | (p,x) <- tail xs', 
                                  let l = filter (\(k,v) -> v < x) $ (take p xs'),
                                  let v = if null l then 0 else maximum $ map ((lis !) . fst) l ]
lisDP xs = reverse . snd . getMax $ elems lis
  where n = length xs
        getMax = maximumBy (comparing fst)
        xs' = zip [0..] xs
        lis :: Array Int (Int, [Int])
        lis = array (0,n-1) $
          [(0,(1,[head xs]))] ++ [ (p, v) | (p,x) <- tail xs', 
                                  let l = filter (\(k,v) -> v < x) $ (take p xs'),
                                  let v = if null l
                                          then (1, [x])
                                          else let (a,b) = getMax $ map ((lis !) . fst) l
                                               in (a+1,x:b) ]
-- lis(i) is smallest ending value of all length-i LISs found so far --> greedy with DC -> nlogk
-- theme is keeping list sorted and apply binary search
-- Index   0  1  2  3  4  5  6  7
-- A      -1 10  9  2  3  8  8  1
{- lis(i) -1 10
              9
              2  3  8
              1                   -}
lisDC xs = runST $ do
  arr <- newArray (0, n-1) (undefined :: (Int,[Int]))
  writeArray arr 0 (head xs, [head xs])
  k <- lisDC' arr 1 (tail xs)
  (reverse . snd . maximumBy (comparing fst) . take k . elems) <$> freeze arr
  where n = length xs

lisDC' :: STArray s Int (Int,[Int]) -> Int -> [Int] -> ST s Int
lisDC' _ n [] = return n
lisDC' arr n (x:xs) = do
--  traceM $ show (n,x)
  p <- bsearch arr 0 n x
  prev <- if p >= 0 then snd <$> readArray arr p
          else return []
  writeArray arr (p+1) (x, x:prev)
--  traceM $ "writeArray " ++ show (p+1)
  lisDC' arr (max n (p+2)) xs

bsearch :: STArray s Int (Int, [Int]) -> Int -> Int -> Int -> ST s Int
bsearch arr s e x = do
--  traceM $ show (s,e,x) ++ " readArray " ++ show (m)
  v <- fst <$> readArray arr m
  case (v >= x, s == m) of
    (True, True)   -> return $ s-1
    (False, True)   -> return $ s
    (True, False)  -> bsearch arr s m x
    (False, False) -> bsearch arr m e x
  where m = (s+e) `div` 2

prop_lis, prop_lis2 :: (NonEmptyList Int) -> Bool
prop_lis xs = length (lisDP (g xs)) == lisLengthDP (g xs)
  where g = getNonEmpty
prop_lis2 xs = (lisDP (g xs)) == lisDC (g xs)
  where g = getNonEmpty
checkLis = do
  quickCheck prop_lis
  quickCheck prop_lis2
  
testLis :: IO ()
testLis = do
  let a = mkRandomInts 1000
  defaultMain
    [ --bench "Lis-Brute2" $ whnf lisBrute' a
     bench "Lis-DP" $ whnf lisDP a
    , bench "Lis-DC" $ whnf lisDC a
    ]

-- 0-1 Knapsack (Subset Sum)
-- Given n items, each with its own value Vi and weight Wi, forall i in [0..n-1],
-- and a maximum knapsack size S, compute the maximum value of the items that
-- we can carry, if we can either ignore or take a particular item.
-- ex. n = 4, V = {100,70,50,10}, W = {10,4,6,12}, S = 12
--     optimal is select item 1 & 2 then total weight 10 and total value 120

subsetSumBrute :: Int -> [(Int,Int)]  -> (Int, [(Int,Int)])
subsetSumBrute 0 _  = (0,[])
subsetSumBrute _ [] = (0,[])
subsetSumBrute rem (x@(v,w):xs)
  | w > rem = subsetSumBrute rem xs
  | otherwise = let (v1,without) = subsetSumBrute rem xs
                    (v2,with)    = (\(p,q) -> (p+v,x:q)) $
                      subsetSumBrute (rem - w) xs
                in if v1 > v2 then (v1,without) else (v2,with)

subsetSumDP :: Int -> [(Int,Int)]  -> (Int, [(Int,Int)])
subsetSumDP m xs = subsetSumDP' m 0
  where n = length xs
        bounds = ((0,0), (m,n))
        xarr = listArray (0,n-1) xs
        arr = listArray bounds [subsetSumDP' i j | (i,j) <- range bounds]
        subsetSumDP' rem k
          | rem == 0 = (0,[])
          | k == n   = (0,[])
          | w > rem  = arr ! (rem,k+1)
          | otherwise = let (v1,without) = arr ! (rem,k+1)
                            (v2,with)    = (\(p,q) -> (p+v,(v,w):q)) $
                              arr ! (rem-w,k+1)
                        in if v1 > v2 then (v1,without) else (v2,with)
          where (v,w) = xarr ! k

testSubsetSum = subsetSumBrute 12 [(100,10),(70,4),(50,6),(10,12)]
testSubsetSum2 n = subsetSumBrute n $ [(100*x,x) | x <- [1..n] ]
testSubsetSum3 n = subsetSumDP n $ [(100*x,x) | x <- [1..n] ]

-- Coin Change (CC)
-- Given a target amount V cents and a list of denominations for
-- n coins, i.e. we have coinValue[i] (in cents) for coin types
-- i in [0..n-1], what is the minimum number of coins that we must use
-- to represent V? Assume that we have unlimited supply of coins of
-- any type.
-- V=7, n=4, coinValue=[1,3,4,5] --> optimal solution is 2 coins [3,4]
-- compute change(value), where value is the remaining amount of cents

coinChangeBrute, coinChangeDP :: [Int] -> Int -> Maybe [Int]
coinChangeBrute coinType n
  | n == 0    = Just []
  | n < 0     = Nothing
  | otherwise = let vs = map (n-) coinType
                    r = minimumBy (comparing (fmap length . snd))
                      . filter (\(x,y) -> y /= Nothing)
                      . zip coinType
                      . map (coinChangeBrute coinType) $ vs
                in (fst r:) <$> snd r

coinChangeDP coinType n = coinChangeDP' n
  where bounds = (0,n)
        arr = listArray bounds [coinChangeDP' i | i <- range bounds]
        coinChangeDP' n
          | n == 0 = Just []
          | n < 0  = Nothing
          | otherwise = let vs = map (n-) coinType
                            r = minimumBy (comparing (fmap length . snd))
                              . filter (\(x,y) -> y /= Nothing)
                              . zip coinType
                              . map (queryArray arr) $ vs
                        in (fst r:) <$> snd r
        queryArray a k
          | k < 0 = Nothing
          | otherwise = a ! k

-- find the number of possible ways to get value V from coinType
--  V = 10, n = 2, coinValue = {1,5}, ways = 3 [ten 1s, five 1s and one 5, two ts]
-- keep ways(type,0)
coinChangeWaysDP :: [Int] -> Int -> Int
coinChangeWaysDP coinType n = dp 0 n
  where m = length coinType
        bounds = ((0,0),(m,n))
        coinArray = listArray (0,m-1) coinType
        arr = listArray bounds [dp i j | (i,j) <- range bounds]
        dp i j
          | j == 0 = 1 -- one way, use nothing
          | j < 0  = 0 -- no way.
          | i >= m = 0 -- no way. we've done all coin type
          | otherwise = arr .! (i+1,j) -- ignore this coin type
                      + arr .! (i, j - (coinArray!i))
        x .! (a,b) | a >= m = 0
                   | b <  0 = 0
                   | otherwise = x ! (a,b)

testCoin = coinChangeBrute [1,2,3,5,10] 10
testCoin2 = coinChangeBrute [1,3,4,5] 7
testCoin3 = coinChangeDP ([1,2,3,5,7] ++ [10,13..100]) 123

-- Traveling Salesperson Problem (TSP)
-- Given n cities and their pairwise distances in the form of a matrix `dist`
-- of size n*n, compute the cost of making a tour that starts from any city s,
-- goes through all the other n-1 cities exactly once, and finally return to the
-- starting city s

mat:: [[Int]]
-- following has solution A-B-C-D-A with a cost 97
--       A  B  C  D
mat = [[ 0,20,42,35]
      ,[20, 0,30,34]
      ,[42,30, 0,12]
      ,[35,34,12, 0]]
genMat :: Int -> [[Int]]
genMat n = replicate n $ replicate n 1
testTSP n = tspBrute n $ genMat n
testTSP' n = tspBrute' n $ genMat n
testTspDP n = tspDP n $ genMat n

tspBrute' :: Int -> [[Int]] -> (Int, [Int])
tspBrute' n dist = minimumBy (comparing fst) route
  where aDist = listArray ((0,0),((n-1),(n-1))) $ concat dist
        go [] = [[]]
        go xs = concatMap (\x -> let xs' = filter (/= x) xs
                                 in map (x:) $ go xs') xs
        route = map (\r -> let r' = r ++ [head r]
                           in (calcDist r', r'))
                $ go [0..n-1]
        calcDist r = sum . zipWith (\x y -> aDist ! (x,y)) r $ tail r

tspBrute :: Int -> [[Int]] -> (Int, [Int])
tspBrute n dist = go 0 1 -- start with node 0
  where aDist = listArray ((0,0),((n-1),(n-1))) $ concat dist
        go :: Int -> Int -> (Int, [Int])
        go pos mask
          | mask == 2^n - 1 = (aDist ! (pos,0), [pos,0])
          | otherwise = let (w,p) = minimumBy (comparing fst) $
                              [ (aDist!(pos,nxt) + w', p')
                              | nxt <- [0..n-1], nxt/=pos, mask .&. (shift 1 nxt) == 0,
                                let (w',p') = go nxt (mask .|. (shift 1 nxt)) ]
                        in (w, pos:p)

tspDP :: Int -> [[Int]] -> (Int, [Int])
tspDP n dist = go 0 1 -- start with node 0
  where aDist = listArray ((0,0),((n-1),(n-1))) $ concat dist
        go :: Int -> Int -> (Int, [Int])
        go pos mask
          | mask == 2^n - 1 = (aDist ! (pos,0), [pos,0])
          | otherwise = let (w,p) = minimumBy (comparing fst) $
                              [ (aDist!(pos,nxt) + w', p')
                              | nxt <- [0..n-1], nxt/=pos, mask .&. (shift 1 nxt) == 0,
                                let (w',p') = dpArr !(nxt, mask .|. (shift 1 nxt)) ]
                        in (w, pos:p)
        bounds = ((0,0),(n,2^n))
        dpArr = listArray bounds [go i j | (i,j) <- range bounds]

-- How do you add?
-- Given an integer n, how many ways can K non-negative integers less than
-- or equal to n add up to n? Constraints: 1 <= n, K <= 100
addWay :: Int -> Int -> Int
addWay _ 1 = 1
addWay n k = sum [ addWay (n-x) (k-1) | x <- [0..n] ]

-- Cutting Sticks
-- Given a stick of length 1<=l<=1000 and 1<=n<=50 cuts to be made to the stick
-- (the cut coordinates, lying in the range [0..l] are given). The cost of cut is
-- determined by the length of the stick to be cut. Your task is to find a cutting
-- sequence so that the overall cost is minimized.
-- ex: l=100,n=3, optimal answer is 200
