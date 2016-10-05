{-
The key points of printf is the ability to either return a String or a function.
Copied from http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/src/Text-Printf.html,

variadicFunction :: VariadicReturnClass r => RequiredArgs -> r
variadicFunction reqArgs = variadicImpl reqArgs mempty

class VariadicReturnClass r where
  variadicImpl :: RequiredArgs -> AccumulatingType -> r

instance VariadicReturnClass ActualReturnType where
  variadicImpl reqArgs acc = constructActualResult reqArgs acc

instance (ArgClass a, VariadicReturnClass r) => VariadicReturnClass (a -> r) where
  variadicImpl reqArgs acc = \a -> variadicImpl reqArgs (specialize a `mappend` acc)
-}
class SumRes r where
  sumOf :: Integer -> r

instance SumRes Integer where
  sumOf = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
  sumOf x = sumOf . (x +) . toInteger

foo = sumOf 1 2 :: Integer
bar = sumOf 1 2 3 4 5 :: Integer
