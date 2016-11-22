-- | Bird's Functional Pearl

-- | KMP / Boyer-Moore string match
import Data.List
import Data.Array
import Debug.Trace
import Text.Printf

matches :: Eq a => [a] -> [a] -> [Int]
matches ws = map length . filter (isSuffixOf ws) . inits

-- | scan lemma
-- | map (foldl op e) . inits = scanl op e
-- | the former is O(n^2), whlie the latter is O(n)
-- |
-- | map/filter rewrite
-- | map f . filter p = map fst . filter snd. map (fork (f,p))
-- | fork (f,p) x = (f x, p x)
-- |
-- | tupling law for foldl
-- | fork (foldl op1 e1, foldl op2 e2) = foldl op (e1,e2) where op (a,b) x = (op1 a x, op2 b x)
-- |
-- | matches ws = map length . filter (isSuffixOf ws) . inits
matches1 ws = map fst . filter snd . map (fork (length, isSuffixOf ws)). inits
  where fork (f,p) x = (f x, p x)

-- | fork (length, isSuffixOf ws) = foldl step (0, e)
-- |   where step (n,x) y = (n+1, op x y)   -- first is length, second is isSuffix
matches2 ws = map fst . filter snd . scanl step (0,e)
  where e = undefined
        op = undefined  -- our object is O(n) op function
        step (n,x) y = (n+1, op x y)

-- | when isSuffixOf has following form: isSuffixOf ws = p. foldl op e
-- | and using
-- | map f . filter (p.g) = map fst . filter (p.snd). map (fork (f,g))
matches3 ws = map fst . filter (p.snd) . scanl step (0,e)
  where e = undefined
        op = undefined  -- our object is O(n) op function
        p = undefined   -- and O(n) p function
        step (n,x) y = (n+1, op x y)

-- | Candidates:
-- | (1) isSuffixOf ws xs = (reverse ws) `isPrefixOf` (reverse xs) --> leads to BM
-- | (2) isSuffixOf ws xs = ws `elem` (tails xs)                   --> leads to KMP

-- | Boyer-Moore
matchesBM1 ws = map fst . filter ((sw `isPrefixOf`).snd) . map (fork (length, reverse)) . inits
  where sw = reverse ws
        fork (f,p) x = (f x, p x)
-- | reverse = foldl (flip (:)) []
matchesBM2 ws = map fst . filter ((sw `isPrefixOf`).snd) . scanl step (0, [])
  where sw = reverse ws
        fork (f,p) x = (f x, p x)
        step (n,sx) x = (n+1, x:sx)
-- | shifting -- hard to understand
shift sw i = head [ k | k <- [1..m], llcp sw (drop k sw) == min i (m-k) ]
  where m = length sw
matchesBM3 ws = test . scanl step (0,[])
  where (sw,m) = (reverse ws, length ws)
        test [] = []
        test ((n,sx):nxs) = if i == m
                            then n:nextIter
                            else nextIter
                            where i = llcp sw sx
                                  k = shift sw i
                                  nextIter = test $ drop (k-1) nxs
        step (n,sx) x = (n+1, x:sx)
-- | final improvement
matchesBM4 ws = test m . scanl step (0,[])
  where (sw,m) = (reverse ws, length ws)
        test _ [] = []
        test j ((n,sx):nxs) | i == m   = n:nextIter k
                            | m-k <= 1 = nextIter k
                            | otherwise = nextIter m
                            where i' = llcp sw (take j sx)
                                  i  = if i' == j then m else i'
                                  k = shift' sw i
                                  nextIter x = test x $ drop (k-1) nxs
        step (n,sx) x = (n+1, x:sx)
        shift' sw i = a ! i
        a = accumArray min m (0,m) (vks ++ vks')
        vks  = zip (allcp' sw) [1..m]
        vks' = zip [m, m-1..1] $ foldr op [] vks
        op (v,k) ks = if v+k == m then k:ks else head ks:ks

-- | Knuth-Moris-Pratt
-- |
-- | isSuffixOf ws = not  null . filter (== ws) . tails   --> cannot translate to foldl
-- | isSuffixOf ws = (== ws) . head . filter (`isPrefixOf` ws). tails  --> (== ws) is O(n) :-(
-- | generalizing filter (`isPrefixOf` ws) . tails  into a function `split`
-- | split ws xs = head [ (us, ws .V. us) | us <- tails xs, us `isPrefixOf` ws ]
-- |   where uv `.V.` u = drop (length u) uv
-- | `split ws xs' splits ws into two lists us and vs so that us ++ vs == ws,
-- | and us is the logest suffix of xs that is a prefix of ws.
-- | split "endnote" "append" == ("end", "note")
-- | Now,
-- | isSuffixOf ws = null . snd . split ws
-- | Let's turn `split' into `foldl'
-- | split ws xs = (us, vs)  ==> split ws (xs ++ [x]) = split ws (us ++ [x])
-- | i.e. the longest suffix of xs++[x] that is a prefix of ws is a suffix of us ++ [x]
-- | split ws xs = if xs `isPrefixOf` ws then (xs, ws .V. xs) else split ws (tail xs)
-- | split ws (xs ++ [x])
-- | = split ws (us ++ [x])
-- | = if us++[x] `isPrefixOf` ws then (us++[x], ws .V. (us++[x]))
-- |   else split ws (tail (us++[x]))
-- | = if [x] `isPrefixOf` vs then (us++[x], tail vs)
-- |   else split ws (tail (us++[x]))
-- | = if [x] `isPrefixOf` vs then (us++[x], tail vs)
-- |   else if null us then ([], ws)
-- |   else split ws (tail us ++ [x])

matchesKMP1 ws = map fst . filter (null.snd.snd) . scanl step (0, ([],ws))
  where step (n, (us,vs)) x = (n+1, op (us,vs) x)
        op (us,vs) x | [x] `isPrefixOf` vs = (us ++ [x], tail vs)
                     | null us             = ([], ws)
                     | otherwise           = op (split ws (tail us)) x
        split ws xs = head [ (us, ws .|. us) | us <- tails xs, us `isPrefixOf` ws ]
        uv .|. u = drop (length u) uv

data Rep a = Null | Node a (Rep a) (Rep a)
matchesKMP ws = map fst . filter (ok.snd) . scanl step (0, root)
  where ok (Node vs l r) = null vs
        step (n,t) x     = (n+1, op t x)
        op Null x        = root
        op (Node [] l r) x     = op l x
        op (Node (v:vs) l r) x = if v == x then r else op l x
        root             = grep Null ws
        grep l []        = Node [] l Null
        grep l (v:vs)    = Node (v:vs) l $ grep (op l v) vs
        

foo ws = filter (`isPrefixOf` ws). tails
-- | aux
llcp :: Eq a => [a] -> [a] -> Int -- length of longest common prefix
llcp [] _ = 0
llcp _ [] = 0
llcp (x:xs) (y:ys)
  | x == y = 1 + llcp xs ys
  | otherwise = 0
allcp xs = [ llcp xs (drop k xs) | k <- [0..length xs - 1] ]
allcp' xs = tail (allcp xs) ++ [0]

test1 = matches "abcab" "ababcabcab"
test2 = matches1 "abcab" "ababcabcab"
testBM1 = matchesBM2 "abcab" "ababcabcab"
testBM2 = matchesBM3 "abcab" "ababcabcab"
testBM3 = matchesBM4 "abcab" "ababcabcab"
testBM42 = length $ matchesBM2 (take 1000 $ repeat 'a') (take 100000 $ repeat 'a')
testBM44 = length $ matchesBM4 (take 1000 $ repeat 'a') (take 100000 $ repeat 'a')
testKMP1 = length $ matchesKMP1 (take 1000 $ repeat 'a') (take 100000 $ repeat 'a')
testKMP =  length $ matchesKMP (take 1000 $ repeat 'a') (take 100000 $ repeat 'a')
testKMP3 = length $ kmpSearch (take 1000 $ repeat 'a') (take 100000 $ repeat 'a')

-- http://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell
-- maybe Morris-Pratt
{-
data MP a = MP { done :: Bool, pat :: [a], next' :: (a -> MP a) }

next :: Show a => MP a -> (a->MP a)
next (MP d p n) = traceShow (printf "next MP(%s,%s)" (show d) (show p) :: String) n
makeTable xs = table
    where table = makeTable' xs (const table)
makeTable' [] failure = MP True [] failure
makeTable' (x:xs) failure = MP False (x:xs) test
    where test c = traceShow (printf "<%s>-<%s>" (show c) (show x) :: String) (if c == x then success else failure c)
          success = makeTable' xs (next (failure x))

isSubstringOfOrg pat text = match (makeTable pat) text
    where match table [] = done table
          match table (y:ys) = done table || match (next table y) ys
isSubstringOf2 pat text = match 0 (makeTable pat) text
    where match n table [] | done table = [n]
          match n table []              = []
          match n table (y:ys) | traceShow (n, y:ys) False = undefined
          match n table (y:ys) | done table = n : match (n+1) (next table y) ys
          match n table (y:ys)              = match (n+1) (next table y) ys
testNat = isSubstringOf2 "abcab" "ababcabcab"
testNat1 = length $ isSubstringOf2 (take 1000 $ repeat 'a') (take 100000 $ repeat 'a')
testNat2 = isSubstringOf2 "abcde" "abcdabcde"
testNat3 = isSubstringOf2 "ababc" "abababc"
testNat4 = isSubstringOf2 "aaa" "aabaabaab"
-}
{-
data MP a = MP { done :: Bool, next :: (a -> MP a) }

makeTable xs = table
    where table = makeTable' xs (const table)
makeTable' [] failure = MP True failure
makeTable' (x:xs) failure = MP False test
    where test c = if c == x then success else failure c
          success = makeTable' xs (next (failure x))
isSubstringOf pat text = match 0 (makeTable pat) text
    where match n table [] | done table = [n]
          match n table []              = []
          match n table (y:ys) | done table = n : match (n+1) (next table y) ys
          match n table (y:ys)              = match (n+1) (next table y) ys
testMP = length $ isSubstringOf (take 10 $ repeat 'a') (take 1000000 $ repeat 'a')
testMP2 = length $ isSubstringOf (take 1000 $ repeat 'a') (take 1000000 $ repeat 'a')

--          match n table (y:ys) | traceShow (n, y:ys) False = undefined
testNat = isSubstringOf "abaa" "ababaa"
-}
-- my try
makeTable :: Eq a => [a] -> Array Int Int
makeTable xs = listArray (0, length xs) $ -1:map findBorder (tail . inits $ xs)
  where findBorder [_] = 0
        findBorder us  = head [ length u | u <- tail (tails us), u `isPrefixOf` us ]
        
kmpSearch :: Eq a => [a] -> [a] -> [Int]
kmpSearch pat text = match 0 0 text
  where m = length pat
        pTable = listArray (0,m-1) pat
        fTable = makeTable pat
        match i j [] | j == m     = [i]
                     | otherwise  = []
        match i j (x:xs) | j < 0         = match (i+1) 0 xs
                         | j == m        = i : match i (fTable!j) (x:xs)
                         | x /= pTable!j = match i (fTable!j) (x:xs)
                         | otherwise     = match (i+1) (j+1) xs

