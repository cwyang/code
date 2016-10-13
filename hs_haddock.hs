-- | Documentation for f
f :: a -> a
f = ...

-- | Multiline documentation for the function
-- f with multiple arguments.
fmap :: Functor f =>
     => (a -> b)  -- ^ function
     -> f a       -- ^ input
     -> f b       -- ^ output

data T a b
  = A a -- ^ Documentation for A
  | B b -- ^ Documentation for B

data R a b = R
  { f1 :: a -- ^ Documentation for the field f1
  , f2 :: b -- ^ Documentation for the field f2
  }

-- element can be hyperlinked with single quotes
data T a b
  = A a -- ^ Documentation for 'A'
  | B b -- ^ Documentation for 'B'

-- module can be refered with double quotes

-- | Here we use the "Data.Text" library and import
-- the 'Data.Text.pack' function.

-- | An example of a code block.
--
-- @
--    f x = f (f x)
-- @

-- | A similar code block example that uses bird tracks (i.e. '>')
-- > f x = f (f x)

-- | Example of an interactive shell session embedded within documentation
--
-- >>> factorial 5
-- 120

module Foo (
  -- * My Header
  example1,
  example2
)

module Foo (
  -- $section1
  example1,
  example2
)

-- $section1
-- Here is the documentation section that describes the symbols
-- 'example1' and 'example2'.

-- URL: <url text>
-- IMAGE: <<diagram.png title>>
-- {-# OPTIONS_HADDOCK show-extensions, ignore-exports #-}       
