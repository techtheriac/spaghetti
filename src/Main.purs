module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Array.Partial (tail)
import Partial.Unsafe (unsafePartial)
import Data.List (List(..))

-- (λx.(λy.x + y))
addMe :: Int -> Int -> Int
addMe x y = x + y

-- (λx.(λy.x + y))(3) -> (λy.3 + y)
addThree :: Int -> Int
addThree = addMe 3

-- Basic Data Constructor
data Foo
  = Foo
  | Bar String

runFoo :: Foo -> String
runFoo Foo = "Damn, right its FoO"

runFoo (Bar s) = "Yeah it's Bar and " <> s

-- Pattern Matching
nonSense :: Int -> Int -> Int
nonSense n 0 = 0

-- K = λx.λy.y
nonSense n _ = n

whoIsGreater :: Int -> Int -> Int
whoIsGreater x y
  | x > y = x
  | otherwise = y

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true

isEmpty _ = false

-- Type Alias 
type PersonRec
  = { name :: String
    , age :: Int
    }

-- Simple Recursion
fact :: Int -> Int
fact 0 = 1

fact n = n * fact (n - 1)

length :: forall a. Array a -> Int
length [] = 0

length arr = 1 + length (unsafePartial tail arr)

-- Algebraic Data Types
data Vehicle
  = Car Wheels
  | MotorCycle Wheels
  | SkateBoard Wheels
  | Bicycle Wheels

data Wheels
  = Wheels Int

instance showWheels' :: Show Wheels where
  show = showWheels

instance showVehicle' :: Show Vehicle where
  show = showVehicle

myVehicle :: Vehicle
myVehicle = Bicycle (Wheels 2)

-- Show Instance for Data Constructor `Wheels`
showWheels :: Wheels -> String
showWheels (Wheels a) = "Wheels: " <> show a

--Show Instance for Data Constructor `Vehicle`
showVehicle :: Vehicle -> String
showVehicle (Car a) = "Vehicle, Car: " <> show a

showVehicle (MotorCycle a) = "Vehicle, MotorCycle: " <> show a

showVehicle (SkateBoard a) = "Vehicle, SkateBoard: " <> show a

showVehicle (Bicycle a) = "Vehicle, Bicycle: " <> show a

-- Lists
myList :: List Int
myList = (Cons 1 (Cons 2 (Cons 3 Nil)))

main :: Effect Unit
main = do
  log "You're such a fucking hoe"
