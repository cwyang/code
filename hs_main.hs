-- My Standard Main

{-# LANGUAGE BangPatterns, OverloadedStrings, QuasiQuotes, InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- Unsafe Extension
{-# OPTIONS_GHC -O2 #-}

import Prelude
import Control.Applicative
import Control.Lens
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Array as A
import qualified Data.Sequence as SQ
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Sequence ((><),(<|),(|>),ViewL((:<)),ViewR ((:>)))
import Data.Array ((!),(//))
import Data.List
import Data.List.Split
import Data.Ord
import Debug.Trace

--import qualified Data.IntMap.Strict as M
import Data.Maybe
import Text.Printf
       
calc :: [Int] -> String
calc _ = "Hi"
main = do
  [n] <- rl
  l <- rl
  putStrLn . calc $ l
  where rl = fmap (map (fst . fromJust . B.readInt) . B.words) B.getLine
        j = 1

-- rdragon's intReader
{-
import Data.IORef
import qualified Data.ByteString.Char8 as B
main :: IO ()
main = d
  rd <- intReader
  [n] <- rd 1
  
intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
-}
