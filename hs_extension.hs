{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map as Map
import GHC.Exts (IsList(..))
import Control.Monad
import Control.Applicative

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
instance (MyClass a) => MyClass [a]

-- With flexible contexts, typeclass contexts can be arbitrary nested types. The
-- following would be forbidden without it.
instance (MyClass (Maybe a)) => MyClass (Either a b)
