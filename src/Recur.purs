module Recur where

import Prelude

import Data.Array (filter, null, tail)
import Data.Maybe (fromMaybe)
--import Data.Traversable (traverseDefault)


lengthArr :: forall a. Array a -> Int
lengthArr arr =
  if null arr
    then 0
    else 1 + (lengthArr $ fromMaybe [] $ tail arr)

-- Execrise 1.
isEvenInt :: Int -> Boolean
isEvenInt x = 
  if x `mod` 2 == 0
  then true
  else false

-- Exercise 2
lengthEvenInt :: Array Int -> Int
lengthEvenInt arr = 
  lengthArr $ filter isEvenInt arr

-- Exercise 3
squareNum :: Array Int -> Array Int
squareNum [] = [0]
squareNum arr = (\x -> x * x) <$> arr

--Exercise 4
infix 8 filter as <$?>
removeNegative :: Array Int -> Array Int 
removeNegative arr = (\x -> x > 0) <$?> arr

