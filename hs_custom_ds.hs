-- Building recursive data structures in Haskell by Duncan Coutts
-- lazy evaluation vs mutable reference
import Data.Array
import Debug.Trace

myRepeat :: a -> [a]
myRepeat x = let xs = x:xs in xs

-- Doubly linked lists

data List a = Node a (List a) (List a)
            | Nil

mkList :: [a] -> List a
mkList xs     = mkList' xs Nil
  where mkList' []     prev = Nil
        mkList' (x:xs) prev = let cur = Node x prev (mkList' xs cur)
                              in cur

-- Graphs, directed, single edge
data DSGraph a = DSGNode a (DSGraph a)
mkDSGraph :: [(a,Int)] -> DSGraph a
mkDSGraph table = table' ! 0
  where n = length table
        table' = listArray (0,n-1) $
          map (\(v,n) -> DSGNode v (table' ! n)) table

-- Graphs, directed
data DGraph a = DGNode a [DGraph a]
mkDGraph :: [(a,[Int])] -> DGraph a
mkDGraph table = table' ! 0
  where n      = length table
        table' = listArray (0,n-1) $
          map(\(v,ns) -> DGNode v (map (table' !) ns)) table

-- my works

-- Graphs
data Graph a = GNode a [Graph a]
mkGraph :: [(a,[Int])] -> Graph a
mkGraph table = table' ! 0
  where n      = length table
        table' = listArray (0,n-1) $
          map(\(x,adjs) -> GNode x (map (table' !) adjs)) table

-- Trees
data Tree a = TNode { value :: a
                    , tlevel :: Int
                    , children ::  [Tree a]
                    } deriving Show
data TreeInfo = Info { parent :: Int
                     , level  :: Int
                     } deriving Show
--mkTree :: [(a,[Int])] -> Array Int (Tree a)

mkTree :: (Num a) => [(a,[Int])] -> Int -> Array Int (Tree a)
mkTree nodes root = treeArray
  where n      = length nodes
        nodeArray = listArray (0,n-1) nodes
        auxArray  = array (0,n-1) $ postorder root 1 root
        preorder parent lvl node
          = let cs = filter (/= parent) $ snd (nodeArray!node)
            in (node, Info {parent = parent, level = lvl})
               : if null cs
                 then []
                 else concatMap (preorder node (lvl+1)) cs
        treeArray = listArray (0,n-1) $
          map (\(node,(v,adjs)) -> let info = auxArray!node
                                       children = map (treeArray!)
                                                $ filter (/= parent info) adjs
                                   in  TNode { value = v
                                             , tlevel = level info
                                             , children =  children
                                             })
          $ zip [0..] nodes
          
foo = mkTree [(100,[1,2]),(200,[0]),(300,[0])]

main = getLine >> getContents >>= mapM_ (putStrLn . show . go) . chunk . lines
  where go :: (Array Int (Tree Int), [(Int,Int)]) -> [Int]
        go (arr,xs)
          | trace (show arr) False = undefined
          | otherwise = map (\(root,target) -> go' arr target $ arr!(root-1)) xs
        go' :: Array Int (Tree Int) -> Int -> Tree Int -> Int
        go' arr target node
          | lvl > target = 0
          | lvl == target = value node
          | otherwise    = sum (map (go' arr target) $ children node)
          where lvl = tlevel node
        chunk :: [String] -> [(Array Int (Tree Int), [(Int,Int)])]
        chunk [] = []
        chunk (xs:ys:zs) = (treeArray, queries): chunk zs''
          where [n,q] = map read . words $ xs
                as = map read . words $ ys
                (es,zs') = splitAt (n-1) zs
                (qs,zs'') = splitAt q zs'
                edgeArray :: Array Int [Int]
                edgeArray = accumArray (\x y -> y:x) [] (0,n-1) $ 
                  concatMap ((\[x,y] -> [(x,y),(y,x)]) . map (pred.read) . words) es
                treeArray = mkTree (zip as $ elems edgeArray) 0
                queries = map ((\[x,y] -> (x,y)) .map read . words) qs
