{-# LANGUAGE RankNTypes #-}
import System.Random
import Control.Monad.State

data Player =
  Player {
  playerName :: String,
  playerPos  :: (Double, Double)
  }
  deriving (Eq, Ord, Show)

type GenAction m = forall a. (Random a) => m a
type GenActionR m = forall a. (Random a) => (a, a) -> m a

genRandom :: (RandomGen g) => GenAction (State g)
genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

{-
randomPlayer
  :: (MonadIO m, MonadState g m, RandomGen g)
     => m Player
randomPlayer = undefined
-}

--randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
  liftIO (putStrLn "Generating random player...")
  
  len <- genR (8, 12)
  name <- replicateM len (genR ('a', 'z'))
  x <- genR (-100, 100)
  y <- genR (-100, 100)

  liftIO (putStrLn "Done.")
  return (Player name (x, y))

run :: IO ()
run = randomPlayer randomRIO >>= print
