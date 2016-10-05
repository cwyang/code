import Control.Arrow
import Control.Monad
import qualified Control.Category as C

newtype SimpleFunc a b = SimpleFunc {
  runF :: (a -> b)
  }

instance Arrow SimpleFunc where
  arr f = SimpleFunc f
  first (SimpleFunc f) = SimpleFunc (mapFst f)
    where mapFst g (a,b) = (g a, b)
  second (SimpleFunc f) = SimpleFunc (mapSnd f)
    where mapSnd g (a,b) = (a, g b)

instance C.Category SimpleFunc where
  (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
  id = arr id

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x,x))
unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry
  -- arr (\op (x,y) -> x `op` y)
liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op
  -- = f &&& g >>> unsplit op

f, g :: SimpleFunc Int Int
f = arr (`div` 2)
g = arr (\x -> x*3 + 1)

h :: SimpleFunc Int Int
h = liftA2 (+) f g

hOutput :: Int
hOutput = runF h 8

newtype MyKleisli m a b = MyKleisli {   -- defined in Control.Monad
  runMyKleisli :: (a -> m b)
  }
instance (Monad m) => C.Category (MyKleisli m) where
  (MyKleisli g) . (MyKleisli f) = MyKleisli (f >=> g)
  id = arr id
instance (Monad m) => Arrow (MyKleisli m) where
  arr f = MyKleisli $ (return . f)
  first (MyKleisli f) = MyKleisli (mapFst f)
    where mapFst famb (a,d) = do
            b <- famb a
            return (b,d)

plusminus, double, h2 :: MyKleisli [] Int Int
plusminus = MyKleisli (\x -> [x, -x])
double    = arr (* 2)
h2        = liftA2 (+) plusminus double

h2Output :: [Int]
h2Output = runMyKleisli h2 8

arrowTest :: IO ()
arrowTest = do
  let
    prepend x = arr (x ++)
    append  x = arr (++ x)
    withId  t = returnA <+> t
    xform = (withId $ prepend "<") >>>
      (withId $ append ">") >>>
      (withId $ ((prepend "!") >>> (append "!")))
    xs = ["test", "foobar"] >>= (runKleisli xform)
  mapM_ putStrLn xs
