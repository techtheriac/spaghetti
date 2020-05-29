module Adt where

import Prelude
import Data.Array (tail)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe)
import Partial.Unsafe (unsafePartial)

-- import Data.Semigroup.Foldable (foldl)
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

-- `unsafePartial` used to silence errors caused by partial functions
-- ALGEBRAIC DATA TYPES (ADTs)
-- Modelling shapes using ADTs
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point
  = Point
    { x :: Number
    , y :: Number
    }

-- Constructing a paricular Line (type `Shape`) using the Line constructor.
-- Algebraic Data Types can be used in constructing Algebraic Data Types.
-- Just like Point is used in cnstructing values for Circle, Rec... in the sum type `Shape`
exampleLine :: Shape
exampleLine = Line p1 p2
  where
  p1 :: Point
  p1 = Point { x: 0.0, y: 0.0 }

  p2 :: Point
  p2 = Point { x: 100.0, y: 50.0 }

-- Consuming ADTs 
-- Converting Shape into a String
-- Employing Pattern Matching
simplePoint :: Point
simplePoint = Point { x: 9.0, y: 7.0 }

-- Exercise : Use ShowShape Function to define a Show instance for type `Shape`
showPoint :: Point -> String
showPoint (Point { x: x, y: y }) = "Coordinates - " <> "x: " <> show x <> " " <> "y: " <> show y

class Show a where
  showShape :: a -> String

instance showShape' :: Show Shape where
  showShape (Circle (Point { x: x, y: y }) r) = "Coordinates - " <> "x: " <> show x <> " " <> "y: " <> show y <> " " <> "radius: " <> show r
  showShape (Rectangle (Point { x: x, y: y }) w h) = "Coordinates - " <> "x: " <> show x <> " " <> "y: " <> show y <> " " <> "width: " <> show w <> " " <> "height: " <> show h
  showShape (Line (Point { x: x1, y: y1 }) (Point { x: x2, y: y2 })) = "x1: " <> show x1 <> " " <> "y1: " <> show y1 <> " " <> "x2: " <> show x2 <> " " <> "y2: " <> show y2
  showShape (Text (Point { x: x, y: y }) text) = "Coordinates - " <> "x: " <> show x <> " " <> "y: " <> show y <> "text: " <> show text

--Create Circle
smallCircle :: Shape
smallCircle = Circle c r
  where
  c :: Point
  c = Point { x: 6.0, y: 0.0 }

  r :: Number
  r = 9.0

-- (Easy) Construct a value of type Shape which represents a circle centered at the origin with radius 10.0.
origin :: Point
origin = Point { x, y }
  where
  x = 0.0

  y = 0.0

bigCircle :: Shape
bigCircle = Circle c r
  where
  c :: Point
  c = origin

  r :: Number
  r = 10.0

--📌(Medium) Write a function from Shape to Shape, which scales its argument by a factor of 2.0, center the origin.
-- NewTypes : A newtype define exactly one constructor
newtype Pixels
  = Pixels Number

newtype Inches
  = Inches Number

-- Defining a type synonym for an Array Of Shapes called Picture
type Picture
  = Array Shape

-- Turning `Picture` into readable String
showPicture :: Picture -> Array String
showPicture = map showShape

pictures :: Picture
pictures = [ bigCircle, exampleLine ]
