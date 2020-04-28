module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

-- (λx.(λy.x + y))
addMe :: Int -> Int -> Int
addMe x y = x + y

-- (λx.(λy.x + y))(3) -> (λy.3 + y)
addThree :: Int -> Int
addThree = addMe 3

-- Basic Data Constructor
data Foo = Foo | Bar String
runFoo :: Foo -> String
runFoo Foo = "Damn, right its FoO"
runFoo (Bar s) = "Yeah it's Bar and " <> s

-- Pattern Matching
nonSense :: Int -> Int -> Int 
nonSense n 0 = 0
-- K = λx.λy.y
nonSense n _ = n

whoIsGreater :: Int -> Int -> Int
whoIsGreater x y | x > y = x
                 | otherwise = y

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

-- Type Alias 
type PersonRec = 
  { name :: String
  , age  :: Int
  }


main :: Effect Unit
main = do
  log "🍝"
