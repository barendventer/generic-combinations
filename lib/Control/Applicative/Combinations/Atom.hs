module Control.Applicative.Combinations.Atom(Atom(..)) where
import Data.Dynamic (Typeable)
import GHC.Generics (Generic)
import Control.Applicative.Combinations.Class

-- | Indicates that combinations method is to treat this as a single field
newtype Atom a = Atom { getAtom :: a } deriving (Eq, Show, Read, Ord, Typeable, Generic)

instance (Applicative f) => Combinations f (Atom a) where combinations = id