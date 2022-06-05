# generic-combinations

This class allows for data structures to be "re-zipped" by an 
 applicative functor. You can define your own instances, or you can use
 `StandaloneDeriving` and `DeriveAnyClass` to create them. `combinations`
 will try to work deeply over nested records and tuples to combine their
 children when possible, but it doesn't attempt to combine the content of 
 variable length structures like lists and maps.

## Example Use

```haskell
--Define the bird facts data structure
data BirdFacts = 
  BirdFacts { 
    flightless :: Bool
    , predator :: Bool
    , size :: BirdSize 
  } 
  deriving (Generic, Show, Eq)

deriving instance (Applicative f) => Combinations f BirdFacts

data BirdSize = Tiny | Small | Large | Huge 
  deriving (Generic, Show, Eq, Enum)

deriving instance (Applicative f) => Combinations f BirdSize

duckFacts, gooseFacts :: BirdFacts 
duckFacts = BirdFacts { flightless = False, predator = True, size = Small }
gooseFacts = BirdFacts { flightless = False, predator = False, size = Large }

--Using combinations with the list functor, we get all combinations of
--the given bird facts, 2^3 as there are two sets of bird facts
combinedFacts = combinations [duckFacts, gooseFacts]

-- combinedFacts ==
--  [BirdFacts {flightless = False, predator = True, size = Small}
--  ,BirdFacts {flightless = False, predator = True, size = Large}
--  ,BirdFacts {flightless = False, predator = False, size = Small}
--  ,BirdFacts {flightless = False, predator = False, size = Large}
--  ,BirdFacts {flightless = False, predator = True, size = Small}
--  ,BirdFacts {flightless = False, predator = True, size = Large}
--  ,BirdFacts {flightless = False, predator = False, size = Small}
--  ,BirdFacts {flightless = False, predator = False, size = Large}]
```

## Motivation

I ran into the need for this while working on an implementation of the harmony
 search optimization algorithm. When the functor is some kind of functor for 
 randomness like `RVar` from `random-fu`, `combinations` represents randomly
 crossing over its given data structures.

## Future Work

Support for `sized`, `fixed-vector`, `fixed-list` and other fixed length 
  data structure libraries.