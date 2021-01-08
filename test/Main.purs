module Test.Main where

import Prelude

import Data.Array (concat, difference, intersect, length, mapMaybe, nubEq, range, replicate, take, union, zipWith)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Performance.Minibench (bench)
import Random.LCG (mkSeed)
import Test.QuickCheck.Gen (Gen, evalGen, shuffle)

-- | Integer division where the result is rounded up.
-- | Intended for unsigned values
divUp :: Int -> Int -> Int
divUp n d = (n + d - 1) / d

-- | Generrates an array of a desired size filled with
-- | duplicates or skipped values based on provided pattern.
-- |
-- | Example:
-- | arrayGen 5 10 [1,0,3]
-- |
-- | Initial values based on desired start:
-- | [5,6,  7  ,8,9,   10   ,11,12,  13   ]
-- | Duplicate counts created by repating provided pattern:
-- | [1,0,  3  ,1,0,    3   ,1 ,0,    3   ]
-- | Initial values duplicated or dropped by pattern:
-- | [5,  7,7,7,8,  10,10,10,11,  13,13,13]
-- | Trimmed result to match output size:
-- | [5,  7,7,7,8,  10,10,10,11,  13]
-- |
-- | Also shuffles pattern assignment, and final ordering.
-- | For example, the above example could have three 5's
-- | scattered throughout the output array.
-- |
arrayGen :: Int -> Int -> Array Int -> Gen (Array Int)
arrayGen start total pattern = do
  let
    cycles = divUp total $ sum pattern
    initialSize = cycles * length pattern
    initial = range start $ start + initialSize - 1
    fullPattern = power pattern cycles

  shuffleInitial <- shuffle initial
  shuffle
    $ take total
    $ concat
    $ zipWith replicate fullPattern shuffleInitial

main :: Effect Unit
main = do
  let
    genState = {newSeed: mkSeed 0, size: 1}
    arr1 = evalGen (arrayGen 0 100 [1,0,5,5]) genState
    arr2 = evalGen (arrayGen 20 10 [1,2,0]) genState

    onlyEven x = if x `mod` 2 == 0 then Just x else Nothing

  logShow arr1
  logShow arr2
  bench1 "mapMaybe" (mapMaybe onlyEven) arr1
  bench1 "mapMaybe" (mapMaybe onlyEven) arr2
  bench1 "nubEq" nubEq arr1
  bench1 "nubEq" nubEq arr2
  bench2 "union" union arr1 arr2
  bench2 "union" union arr2 arr1
  bench2 "intersect" intersect arr1 arr2
  bench2 "intersect" intersect arr2 arr1
  bench2 "difference" difference arr1 arr2
  bench2 "difference" difference arr2 arr1


bench1 :: forall a b. String -> (Array a -> b) -> Array a -> Effect Unit
bench1 label func arr = do
  log $ label <> " (" <> show (length arr) <> ")"
  bench \_ -> func arr

bench2 :: forall a b. String -> (Array a -> Array a -> b) -> Array a -> Array a -> Effect Unit
bench2 label func arr1 arr2 = do
  log $ label <> " (" <> show (length arr1) <> ", " <> show (length arr2) <> ")"
  bench \_ -> func arr1 arr2
