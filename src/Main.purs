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

data Foo = Foo | Bar String
runFoo :: Foo -> String
runFoo Foo = "Damn, right its FoO"
runFoo (Bar s) = "Yeah it's Bar and " <> s


main :: Effect Unit
main = do
  log "🍝"
