module Control.Applicative.Combinations.Class where

class Combinations f a where
  -- | Map some selector from `a` and apply the resulting `f` idiom to each matching field for the `a` constructor
  combinations :: f a -> f a