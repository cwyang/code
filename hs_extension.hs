{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
-- maybe default. {-# LANGUAGE TypeSynonymInstances #-}
-- deprecated from 7.10 {-# LANGUAGE OverlappingInstances #-}

import qualified Data.Map as Map
import GHC.Exts (IsList(..))
import Control.Monad
import Control.Applicative
import qualified Data.Text as T
-- Rose Tree
data Tree a = Node a [Tree a]
  deriving (Show, Functor, Foldable, Traversable)

-- Overloaded List
{-
instance (Ord k) => IsList (Map.Map k v) where
  type Item (Map.Map k v) = (k,v)
  fromList = Map.fromList
  toList = Map.toList
-}
class MyIsList l where
  type MyItem l
  fromList  :: [MyItem l] -> l
  toList    :: l -> [MyItem l]

instance MyIsList [a] where
  type MyItem [a] = a
  fromList = id
  toList  = id

example1 :: Map.Map String Int
example1 = [("a", 1), ("b", 2)]

-- Flexible Instance

class MyClass a

-- Without flexible instances, all instance heads must be type variable. The
-- following would be legal.
instance MyClass (Maybe a)

-- With flexible instances, typeclass heads can be arbitrary nested types. The
-- following would be forbidden without it.
instance MyClass (Maybe Int)

-- Without flexible contexts, all contexts must be type variable. The
-- following would be legal.
instance (MyClass a) => MyClass [Maybe a]

-- With flexible contexts, typeclass contexts can be arbitrary nested types. The
-- following would be forbidden without it.
instance (MyClass (Maybe a)) => MyClass (Either a b)

--
class MyClass1 a b where
    fn :: (a,b)

instance {-# OVERLAPPING #-} MyClass1 Int b where
  fn = error "b"

instance {-# OVERLAPPING #-} MyClass1 a Int where
  fn = error "a"

instance {-# OVERLAPPING #-} MyClass1 Int Int where
  fn = error "c"

example :: (Int, Int)
example = fn

--
type IntList = [Int]

-- Without type synonym instances, we're forced to manually expand out type
-- synonyms in the typeclass head.
--instance MyClass [Int]

-- With it GHC will do this for us automatically. Type synonyms still need to
-- be fully applied.
instance MyClass IntList

--
sum :: Num a => [a] -> a
sum = go 0
  where
    go !acc (x:xs) = go (acc + x) xs
    go  acc []    = acc
--

{-# LANGUAGE StrictData #-}

data Employee = Employee
                { name :: T.Text
                , age :: Int
                }
