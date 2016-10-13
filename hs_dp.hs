-- Longest Increasing Subsequence (LIS)

import System.Random
import Data.Array
import Data.List
import Data.Ord
import Criterion.Main
import Debug.Trace
import Test.QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList (..))

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
    
lisBrute, lisBrute', lisDP, lisDP' :: [Int] -> [Int]

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
lisDP xs = reverse. snd . getMax $ elems lis
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
lisDP' xs = snd . getMax $ elems lis
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

prop_lis :: (NonEmptyList Int) -> Bool
prop_lis xs = length (lisDP' (g xs)) == lisLengthDP (g xs)
  where g = getNonEmpty
checkLis = do
  quickCheck prop_lis
  
testLis :: IO ()
testLis = do
  let a = mkRandomInts 50
  defaultMain
    [ bench "Lis-Brute2" $ whnf lisBrute' a
    , bench "Lis-DP" $ whnf lisDP a
    ]
