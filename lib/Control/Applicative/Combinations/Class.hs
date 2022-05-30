{-# LANGUAGE DeriveAnyClass #-}
module Control.Applicative.Combinations.Class where
import GHC.Generics
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Data.Map(Map)
import Data.Set(Set)
class (Applicative f) => Combinations f a where
  -- | Distributes the idiom over the fields of `a` and applies the constructor for `a` to the resulting idioms
  -- LAWS: 
  -- `combinations . pure = pure`
  combinations :: f a -> f a
  default combinations :: (Functor f, Generic a, GCombinations f (Rep a)) => f a -> f a
  combinations = fmap to . gcombinations . fmap from

class GCombinations f a where
  -- | Distributes the idiom over the fields of `a` and applies the constructor for `a` to the resulting idioms 
  gcombinations :: f (a p) -> f (a p)

instance GCombinations f V1 where
  gcombinations = id

instance GCombinations f U1 where
  gcombinations = id

-- In principle, sum types could be supported if Columns a ~ Columns b.
-- This is probably not expressible in Haskell, and it would make the context stricter. 
-- Better to have the user be explicit if they want that behavior, by using an isomorphic tuple with enum tag.
instance GCombinations f (a :+: b) where
  gcombinations = id

instance (Functor f, Combinations f c) => GCombinations f (K1 i c) where
  gcombinations = fmap K1 . combinations . fmap unK1

instance (Functor f, GCombinations f c) => GCombinations f (M1 i t c) where
  gcombinations = fmap M1 . gcombinations . fmap unM1

instance (Applicative f, GCombinations f a, GCombinations f b) => GCombinations f (a :*: b) where
  gcombinations fab = (:*:) <$> fa <*> fb where
    fa = gcombinations $ fmap (\(a :*: _) -> a) fab
    fb = gcombinations $ fmap (\(_ :*: b) -> b) fab

-- Derived instances
deriving instance (Combinations f a, Combinations f b, Applicative f) => Combinations f (a,b)
deriving instance (Combinations f a, Combinations f b, Combinations f c, Applicative f) => Combinations f (a,b,c)
deriving instance (Combinations f a, Combinations f b, Combinations f c, Combinations f d, Applicative f) => Combinations f (a,b,c,d)
deriving instance (Combinations f a, Combinations f b, Combinations f c, Combinations f d, Combinations f e, Applicative f) => Combinations f (a,b,c,d,e)
deriving instance (Combinations f a, Combinations f b, Combinations f c, Combinations f d, Combinations f e, Combinations f g, Applicative f) => Combinations f (a,b,c,d,e,g)
deriving instance (Combinations f a, Combinations f b, Combinations f c, Combinations f d, Combinations f e, Combinations f g, Combinations f h, Applicative f) => Combinations f (a,b,c,d,e,g,h)
deriving instance (Combinations f a, Functor f) => Combinations f (Identity a)

-- Trivial derived instances for atomic data types
deriving instance (Applicative f) => Combinations f Void
deriving instance (Applicative f) => Combinations f ()
deriving instance (Applicative f) => Combinations f Bool

-- Trivial instances for non-generic primitive atoms
instance (Applicative f) => Combinations f Char where combinations = id
instance (Applicative f) => Combinations f Int where combinations = id
instance (Applicative f) => Combinations f Float where combinations = id
instance (Applicative f) => Combinations f Double where combinations = id
instance (Applicative f) => Combinations f Integer where combinations = id

-- These are trivial because they are variable length, so there is no notion of a "column"
instance (Applicative f) => Combinations f [a] where combinations = id
instance (Applicative f) => Combinations f (Map k a) where combinations = id
instance (Applicative f) => Combinations f (Set a) where combinations = id

-- TODO: Instances for fixed length vectors