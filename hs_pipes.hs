-- echo.hs

import Control.Monad (unless, guard)
import Control.Monad.Trans.Maybe
import Pipes
import qualified Pipes.Prelude as P
import System.IO (isEOF)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import Control.Monad.Identity


 --         +--------+-- A 'Producer' that yields 'String's
 --         |        |
 --         |        |      +-- Every monad transformer has a base monad.
 --         |        |      |   This time the base monad is 'IO'.
 --         |        |      |
 --         |        |      |  +-- Every monadic action has a return value.
 --         |        |      |  |   This action returns '()' when finished
 --         v        v      v  v
stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
  unless eof $ do
    str <- lift getLine
    yield str            -- 'yield' the 'String'
    stdinLn              -- Loop

loop :: Effect IO ()
loop = for stdinLn $ \str -> do  -- Read this like: "for str in stdinLn"
  lift $ putStrLn str          -- The body of the 'for' loop

run :: IO ()
run = runEffect loop



--          +--------+-- A 'Consumer' that awaits 'String's
--          |        |
--          v        v
stdoutLn :: Consumer String IO ()
stdoutLn = do
  str <- await  -- 'await' a 'String'
  x   <- lift $ try $ putStrLn str
  case x of
    -- Gracefully terminate if we got a broken pipe error
    Left e@(G.IOError { G.ioe_type = t}) ->
      lift $ unless (t == G.ResourceVanished) $ throwIO e
    -- Otherwise loop
    Right () -> stdoutLn

doubleUp :: Monad m => Consumer String m String
doubleUp = do
  str1 <- await
  str2 <- await
  return (str1 ++ str2)
 -- more concise: doubleUp = (++) <$> await <*> await

run2 :: IO ()
run2 = runEffect $ lift getLine >~ doubleUp >~ stdoutLn

run3 :: IO ()
run3 = do
  runEffect $ P.stdinLn >-> P.stdoutLn


input :: Producer String IO ()
input = P.stdinLn >-> P.takeWhile (/= "quit")

name :: ListT IO String
name = do
  firstName <- Select input
  lastName  <- Select input
  return (firstName ++ " " ++ lastName)

run4 = runEffect $ every name >-> P.stdoutLn

customerService :: Producer String IO ()
customerService = do
  each [ "Hello, how can I help you?"      -- Begin with a script
       , "Hold for one second."
       ]
  P.stdinLn >-> P.takeWhile (/= "Goodbye!")  -- Now continue with a human

{-
ii :: MaybeT IO ()
ii = do
  str <- lift getLine
  guard (str /= "Fail")

run5 = runEffect $ every input >-> P.stdoutLn
-}

a :: Producer Int Identity ()
a = forM_ [1..10] yield

b :: Pipe Int Int Identity ()
b =  forever $ do
  x <- await
  yield (x*2)
  yield (x*3)
  yield (x*4)

c :: Pipe Int Int Identity ()
c = forever $ do
  x <- await
  if (x `mod` 2) == 0
    then yield x
    else return ()

result = P.toList $ a >-> b >-> c >-> P.map show
