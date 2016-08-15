{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hspec.Hashable (

  -- * Main functions
  -- $main
    testHashableUniqueness
  , testHashableUniquenessWithoutTypeable
  , testSelfEquality
  , testHashableCollision

  -- * Internal help functions
  -- $helperfunctions
  , dupsByMatchingSnd
  ) where

import Control.Arrow ((&&&))

import Data.Hashable
import Data.List
import Data.Proxy
import Data.Typeable

import Test.Hspec
import Test.QuickCheck


-- | For every 'Hashable' instance of a type, each unique value of that type
-- should have a unique hash. Generally, a 'Generic' 'Hashable' instance of a type should
-- create a unique hash, and ideally these match the rules of type's 'Eq'
-- instance. Any values for that type that are equal should have the same hash
-- and any values that are not equal should have unique hashes.
-- There might still be cases where a 'Generic' 'Hashable' instance
-- breaks those expectations. There are also cases where you might implement
-- 'Hashable' by hand. This testing library assumes that you expect the
-- uniqueness of a type matches in `Eq` and `Hashable`.



-- $main

-- | the main testing function, give it a sampleSize larger than zero (or it will fail) and it
-- will produce arbitrary elements to test the uniqueness of the created hash
-- for a particular type. Should use a large sample size to help find hash collisions.
testHashableUniqueness :: forall a. (Arbitrary a, Eq a, Hashable a, Show a, Typeable a)
  => Int -> Proxy a -> Spec
testHashableUniqueness sampleSize proxy = do
  case sampleSize <= 0 of
    True -> fail ("The sample size must be greater than zero. The sample size you provided is: " ++ show sampleSize ++ ".")
    False -> do
      testSelfEquality sampleSize typeName proxy
      testHashableCollision sampleSize typeName proxy
  where
    typeName = show . typeRep $ proxy

testHashableUniquenessWithoutTypeable :: forall a. (Arbitrary a, Eq a, Hashable a, Show a)
  => Int -> String -> Proxy a -> Spec
testHashableUniquenessWithoutTypeable sampleSize typeName proxy = do
  case sampleSize <= 0 of
    True -> fail ("The sample size must be greater than zero. The sample size you provided is: " ++ show sampleSize ++ ".")
    False -> do
      testSelfEquality sampleSize typeName proxy
      testHashableCollision sampleSize typeName proxy

-- | test whether or not the Eq instances is defined such that any value
-- equals itself. If it does not, then the testHashableCollision
-- testing function might not work as expected.
testSelfEquality :: forall a. (Arbitrary a, Eq a, Hashable a, Show a)
  => Int -> String -> Proxy a -> Spec
testSelfEquality sampleSize typeName Proxy =
  describe ("Values of " ++ typeName ++ " derive Eq.") $
    it "all values should be equal to themself. " $ do
      xs <- generate (vectorOf sampleSize (arbitrary :: Gen a))
      (and $ (\x -> x == x) <$> xs) `shouldBe` True

-- | test whether or not there is are hash collisions between unique values.
-- if there are you need to fix your definition of Hashable.
testHashableCollision :: forall a. (Arbitrary a, Eq a, Hashable a, Show a)
  => Int -> String -> Proxy a -> Spec
testHashableCollision sampleSize typeName Proxy =
  describe ("Hashed values of " ++ typeName) $
    it "all non-equivalent values should have unique hashes" $ do
      xs <- generate (vectorOf sampleSize (arbitrary :: Gen a))
      -- nub : remove duplicates in xs
      -- (id &&& hash): put x and hash of x in a tuple
      -- dupsByMatchingSnd: get any tuples that have the same hash value but
      -- have unique (non-equivalent) x values.
      let matchingHashesForUniqueXs = dupsByMatchingSnd [] $ (id &&& hash) <$> nub xs
      -- if the Eq and Hashable instances are well defined, the list should be empty
      matchingHashesForUniqueXs `shouldBe` []


-- $helperfunctions
-- | filter a list by collecting all duplications of the second item of
-- the tuple and return both elements of the tuple.
dupsByMatchingSnd :: (Eq b) => [(a,b)] -> [(a,b)] -> [(a,b)]
dupsByMatchingSnd ys (x:xs) = newX ++ dupsByMatchingSnd (ys ++ [x]) xs
  where
    xDups = filter (\y -> (snd x) == (snd y)) (xs ++ ys)
    newX  = if length xDups > 0
              then [x]
              else []
dupsByMatchingSnd _  []     = []
