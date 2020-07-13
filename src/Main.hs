-- TODO: more than one player
-- TODO: community chest jail?
-- TODO: learn things about the monopoly world
-- TODO: what the heck is going on with mh?

module Main where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Traced
import qualified Data.MultiSet as MS
import GHC.Show (show)

newtype Square = Square (Either Int Int)
  deriving (Eq, Ord, Generic)

pattern Sq :: Int -> Square
pattern Sq a = Square (Right a)

pattern Jail :: Int -> Square
pattern Jail a = Square (Left a)

{-# COMPLETE Sq, Jail #-}

main :: IO ()
main = do
  squareSamples <- take 5000 <$> (sampleIO $ prior $ mh nsamples (playerSquare 20))
  print (freq squareSamples)

-- | Distribution over the player's current square after n rolls
playerSquare :: MonadInfer m => Int -> m Square
playerSquare n = do
  let rolls = nTwoRoll n
  rolls' <- sequence rolls
  foldM moveTo (Sq 0) rolls'

moveTo :: MonadInfer m => Square -> (Int, Int) -> m Square
moveTo (Jail 1) _ = pure (Sq 10)
moveTo (Jail n) (roll1, roll2)
  | roll1 == roll2 = pure (Sq 10)
  | otherwise = pure (Jail (n - 1))
moveTo (Sq start) (roll1, roll2) = do
  let newSquare = (start + roll1 + roll2) `mod` 40
  let drawChance = newSquare `elem` chanceSquares
  goToJail <- (drawChance &&) <$> chanceJail
  pure $
    if goToJail
    then Jail 3
    else Sq newSquare

nsamples :: Int
nsamples = 10000

freq :: Ord a => [a] -> [(a, Int)]
freq = MS.toOccurList . MS.fromList

chanceJail :: MonadSample m => m Bool
chanceJail = do
  x <- uniform 0 1
  pure (x < 1 / 2)

diceRoll :: MonadSample m => m Int
diceRoll = uniformD [1 .. 6]

nDiceRoll :: MonadSample m => Int -> m [Int]
nDiceRoll n = sequence (take n (repeat diceRoll))

twoRoll :: MonadSample m => m (Int, Int)
twoRoll = (,) <$> diceRoll <*> diceRoll

nTwoRoll :: MonadSample m => Int -> [m (Int, Int)]
nTwoRoll n = take n (repeat twoRoll)

allSquares :: [Int]
allSquares = [0 .. 39] -- 0 = GO

chestSquares :: [Int]
chestSquares = [2, 17, 33]

chanceSquares :: [Int]
chanceSquares = [7, 22, 36]

count :: Eq a => a -> [a] -> Int
count = length ... filter . (==)

instance Show Square where
  show (Jail jailTime) = "Jail " <> GHC.Show.show jailTime
  show (Sq square) = GHC.Show.show square
