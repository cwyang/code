import Data.Tree
import qualified Data.Vector.Unboxed as V
import GHC.Prim
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Vector.Unboxed (freeze)
import Data.Vector.Unboxed.Mutable
import qualified Data.HashSet as S
import qualified Data.HashMap.Lazy as M
import Prelude hiding (lookup)
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as H
import qualified Data.DList as DL
import Control.Monad
import Control.Monad.Writer



{-

   A
  / \
 B   C
    / \
   D   E

-}

tree :: Tree String
tree = Node "A" [Node "B" [], Node "C" [Node "D" [], Node "E" []]]

postorder :: Tree a -> [a]
postorder (Node a ts) = elts ++ [a]
  where elts = concat (map postorder ts)

preorder :: Tree a -> [a]
preorder (Node a ts) = a : elts
  where elts = concat (map preorder ts)

ex1 = drawTree tree
ex2 = drawForest (subForest tree)
ex3 = flatten tree
ex4 = levels tree
ex5 = preorder tree
ex6 = postorder tree

norm ::  V.Vector Double -> Double
norm = sqrt . V.sum . V.map (\x -> x*x)

example1 :: Double
example1 = norm $ V.iterateN 1000000 (+1) 0.0


example :: PrimMonad m => m (V.Vector Int)
example = do
    v <- new 10
    forM_ [0..9] $ \i ->
      write v i (2*i)
    freeze v

          -- vector computation in IO
vecIO :: IO (V.Vector Int)
vecIO = example

-- vector computation in ST
vecST :: ST s (V.Vector Int)
vecST = example

vecSTTest :: IO ()
vecSTTest = do
  vecIO >>= print
  print $ runST vecST


exampl1 :: M.HashMap Int Char
exampl1 = M.fromList $ Prelude.zip [1..10] ['a'..]

exampl2 :: S.HashSet Int
exampl2 = S.fromList [1..10]

-- Hashtable parameterized by ST "thread"
type HT s = H.HashTable s String String

set :: ST s (HT s)
set = do
  ht <- H.new
  H.insert ht "key" "value1"
  return ht

get :: HT s -> ST s (Maybe String)
get ht = do
  val <- H.lookup ht "key"
  return val

exampl :: Maybe String
exampl = runST (Main.set >>= get)


logger :: Writer (DL.DList Int) ()
logger = replicateM_ 1000 $ tell (DL.singleton 0)
