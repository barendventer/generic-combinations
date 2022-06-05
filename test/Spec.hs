{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Control.Applicative.Combinations
import GHC.Generics (Generic)
import Test.Hspec

-- | The different options for pet species
data PetSpecies = Rabbit | Parrot | Ferret | Guppy deriving (Eq, Ord, Show, Read, Enum, Generic)

deriving instance (Applicative f) => Combinations f PetSpecies

-- | The statistics a pet can have. Provided to test that combinations can traverse deep structures.
data PetStats = PetStats {age :: Int, favoriteFoods :: [String], species :: PetSpecies} deriving (Eq, Ord, Show, Read, Generic)

deriving instance (Applicative f) => Combinations f PetStats

-- | A pet with name, age, favorite foods, and species
data Pet = Pet {name :: String, stats :: PetStats, velocity :: Atom (Double, Double)} deriving (Eq, Show, Read, Ord, Generic)

deriving instance (Applicative f) => Combinations f Pet

-- | A list of pets with specific properties.
-- Property: it doesn't contain a guppy
-- Property: all the pets have a zero in their velocity tuple
-- Property: none of the pets have the same number of favorite foods
petList, petListCombinations :: [Pet]
petList =
  [ Pet {name = "Trixie", stats = PetStats {age = 2, favoriteFoods = ["carrot", "lettuce", "carpet"], species = Rabbit}, velocity = Atom (0, 0)},
    Pet {name = "Carpet", stats = PetStats {age = 3, favoriteFoods = ["mealworm", "buffalo wings"], species = Ferret}, velocity = Atom (1, 0)},
    Pet {name = "Papaya", stats = PetStats {age = 12, favoriteFoods = ["betel nut"], species = Parrot}, velocity = Atom (0, 2)}
  ]
petListCombinations = combinations petList

--An example combination of the input pets
exampleCombinedPet :: Pet
exampleCombinedPet = Pet {name = "Papaya", stats = PetStats {age = 12, favoriteFoods = ["mealworm", "buffalo wings"], species = Parrot}, velocity = Atom (0, 2)}

--Predicates on pets
hasZeroInVector, notAGuppy, hasAtLeastOneFavoriteFood :: Pet -> Bool
hasZeroInVector (Pet {velocity = Atom (x, y)}) = x == 0 || y == 0
notAGuppy (Pet {stats = PetStats {species}}) = species /= Guppy
hasAtLeastOneFavoriteFood (Pet {stats = PetStats {favoriteFoods}}) = not $ null favoriteFoods

--The number of expected combinations
petListLength, petStatsRecordSize, petRecordSize, nPetCombinations :: Int
petListLength = length petList --3
petStatsRecordSize = 3 --age, favorite foods, and species
petRecordSize = petStatsRecordSize + 2 --name, velocity
nPetCombinations = petListLength ^ petRecordSize --3^5 == 243

--Tuples with nesting to test tuple instances
tupleList, tupleListCombinations :: [((Double, Double), (Double, Double, (Double, Double)), Double, Double)]
tupleList = [((0, 0), (0, 0, (0, 0)), 0, 0), ((1, 1), (1, 1, (1, 1)), 1, 1)]
tupleListCombinations = combinations tupleList

main :: IO ()
main = hspec $ do
  describe "petListCombinations" $ do
    it "has the expected length based on number of combinations" $ do
      length petListCombinations `shouldBe` nPetCombinations
    it "does not contain a guppy since it wasn't given one to generate combinations from" $ do
      petListCombinations `shouldSatisfy` all notAGuppy
    it "doesn't have any velocity vector without a zero since velocities were atomic and couldn't be recombined" $ do
      petListCombinations `shouldSatisfy` all hasZeroInVector
    it "gives each pet at least one favorite food since no inputs had no favorite foods" $ do
      petListCombinations `shouldSatisfy` all hasAtLeastOneFavoriteFood
    it "contains a known combination that wasn't included in the original inputs" $ do
      petListCombinations `shouldSatisfy` elem exampleCombinedPet
  describe "tupleListCombinations" $ do
    it "is the combinations of 8 bits, and thus has 256 elements" $ do
      length tupleListCombinations `shouldBe` 256
    it "contains the combination ((0, 1), (1, 1, (0, 1)), 0, 1)" $ do
      tupleListCombinations `shouldSatisfy` elem ((0, 1), (1, 1, (0, 1)), 0, 1)