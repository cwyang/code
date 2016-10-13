{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
import Debug.Trace

unsafe :: Num a => Maybe a -> Maybe a
unsafe (Just x) = Just $ x + 1

bar :: Int -> String
bar x = show x

main :: IO ()
main = do
  let a = bar 1
  traceM "hi"
  traceShowM a
  putStrLn a
  putStrLn "ho"
  return ()
