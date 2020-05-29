module Recur where

import Prelude
import Data.Foldable (product)
import Data.Array (filter, null, tail, (..), concatMap)
import Data.Maybe (fromMaybe)

--import Data.Traversable (traverseDefault)
lengthArr :: forall a. Array a -> Int
lengthArr arr =
  if null arr then
    0
  else
    1 + (lengthArr $ fromMaybe [] $ tail arr)

-- Execrise 1.
isEvenInt :: Int -> Boolean
isEvenInt x =
  if x `mod` 2 == 0 then
    true
  else
    false

-- Exercise 2
lengthEvenInt :: Array Int -> Int
lengthEvenInt arr = lengthArr $ filter isEvenInt arr

-- Exercise 3
squareNum :: Array Int -> Array Int
squareNum [] = [ 0 ]

squareNum arr = (\x -> x * x) <$> arr

--Exercise 4
infix 8 filter as <$?>

removeNegative :: Array Int -> Array Int
removeNegative arr = (\x -> x > 0) <$?> arr

-- Array Comprehension
factors :: Int -> Array (Array Int)
factors n =
  filter (\xs -> product xs == n)
    $ do
        i <- 1 .. n
        j <- i .. n
        pure [ i, j ]

pairs :: Int -> Array Int
pairs n = concatMap (\i -> 1 .. n) (1 .. n)

--Musing. Figure out what's going on here. It's a good mental exercise
-- map (\x -> foldl (+) 0 x) $ concat $ map (\x -> [1 .. 5]) (1 .. 7)
-- Folds 
{-
:type foldr 
  forall a b f. Foldable f => (a -> b ->  b) -> a -> fa -> b
  f - has a type class constraint of Foldable
  foldr is a higher order function that takes a function of type (a -> b -> b)
    function (a -> b -> b) is a function that takes a value of type a (an accumulator),
    Takes another value of type b (next) and returns a value of type b 
  Having taken the function to Operate on the Functor,
  foldr takes an Initial Value type a
  foldr takes the functor to operate on 
  foldr returns the a value that has the same type as the Initial Value
-}
-- arrayToString arr = foldr (\acc next -> acc <> show next) "" arr
