module Adt where

import Prelude
import Data.Array (tail)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)
import Partial.Unsafe (unsafePartial)

--import Data.Semigroup.Foldable (foldl)
-- import Data.Foldable (foldl)
-- import Global as Global
-- import Math as Math
-- Pattern Matching Intro
-- Greatest Common Divisor
gcd' :: Int -> Int -> Int
gcd' n 0 = n

gcd' 0 m = m

gcd' n m =
  if n > m then
    gcd' (n - m) m
  else
    gcd' n (m - n)

-- Simple Patterns
fromString :: String -> Boolean
fromString "true" = true

fromString _ = false

toString :: Boolean -> String
toString true = "true"

toString false = "false"

-- Guards
whoIsGreater :: Int -> Int -> Int
whoIsGreater x y
  | x > y = x
  | otherwise = y

-- Exercise : Factorial Function
factorial :: Int -> Int
factorial 0 = 0

factorial 1 = 1

factorial n = n * factorial (n - 1)

-- Array isEmpty
isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true

isEmpty _ = false

{- Returns the product of the third and fourth elements 
   of an array of length `5` iff the first and
   Second are 0 and 1 respectively otherweise return 0
-}
takeFive :: Array Int -> Int
takeFive [ 0, 1, a, b, _ ] = a * b

takeFive _ = 0

-- Record Patterns / Row Polymorphism
-- Matches records with the fields `first` and `last` 
-- Binds their values to names `x` and `y` respectively
showPerson :: { first :: String, last :: String } -> String
showPerson { first: x, last: y } = y <> " " <> x

-- 📌 The inferred Type of the folowing 👇 
-- showPerson { first: x, last: y } = y <> " " <> x is 
-- forall r . {first :: String, last :: String | r}
-- It's read as:
-- showPerson takes a record of first and last fields which are Strings
-- And any other fields and returns a String 
-- This function is a Polymorphic function in the row r of record fields
-- Supplying the function with a record of the show below is valid :
-- {first: "Assin", last : "Ass", location : "Pornhub"}
-- Nested Patterns
type Address
  = { street :: String
    , city :: String
    }

type Person
  = { name :: String
    , address :: Address
    }

livesInLa :: Person -> Boolean
livesInLa { address: { city: "Los Angeles" } } = true

livesInLa _ = false

-- Exercise : Write a funtion that uses record patterns to test if two persons belong
--            To the same city  
sameCity :: Person -> Person -> Boolean
sameCity { address: { city: y } } { address: { city: x } } =
  if y == x then
    true
  else
    false

-- fromSingleton :: forall a . a -> Array a -> a
-- Computes the longest suffix which sums to zero
lzs :: Array Int -> Array Int
lzs [] = []

lzs xs = case sum xs of
  0 -> xs -- Return 0 if tail sums to 0
  _ -> lzs (fromMaybe [] $ tail xs) -- Otherwise return the tail of the array

partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true
