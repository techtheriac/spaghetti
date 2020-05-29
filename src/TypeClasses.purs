module TypeClasses where

import Prelude

-- Implementation of `Show` type class
-- The function `show` is defined by a type class in the Prelude module `Show`
-- 👇 Declares a new type class called show parameterized by a type variable a
--class Show a where
--  show :: a -> String
-- A TypeClass instance contains the implementations of the functions defined in a Type class
-- Specialized to a particular type.
-- Definition of show TypeClass Instance for Boolean values
{-
instance showBoolean :: Show Boolean where
 show true = "true"
  show false = "false"

-}
-- In Purescript, we say that the Boolean type belongs to the Show TypeClass.
newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

-- Define an Eq instance for Complexi

--instance Eq' :: Eq Complex where


-- Type Class Constraints
-- A Function that tests if three values are equal
-- 'Eq a' introduced as a Type Class Constraint
-- The Type Signature states that we can call the function with any type `a`
-- As long as there is an Eq instance for `a` in any of the imported modules.

threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual x y z = x == y && y == z

