{-# LANGUAGE DeriveGeneric #-}

module Test.Hspec.HashableSpec where

import           Control.Monad

import           Data.Hashable
import           Data.Proxy
import           Data.Tuple
import           Data.Typeable

import           GHC.Generics

import           System.IO.Silently

import           Test.Hspec
import           Test.Hspec.Hashable
import           Test.Hspec.Core.Runner
import           Test.QuickCheck hiding (output)

hspecSilently :: Spec -> IO (Summary, String)
hspecSilently s = swap <$> capture (hspecResult s)

shouldTestAs :: Spec -> Summary -> IO ()
shouldTestAs testspec expected = do
  (summary, output) <- hspecSilently testspec
  when (summary /= expected) $
    expectationFailure $
      "spec didn't yield the expected summary:\n" ++
      "  expected: " ++ show expected ++ "\n" ++
      "  got:      " ++ show summary ++ "\n" ++
      "output of the test-suite:\n" ++
      indent output

shouldProduceFailures :: Spec -> Int -> IO ()
shouldProduceFailures testspec expected = do
  (summary, output) <- hspecSilently testspec
  when (summaryFailures summary /= expected) $
    expectationFailure $
      "spec didn't yield the expected number of failures summary:\n" ++
      "  expected: " ++ show expected ++ "\n" ++
      "  got:      " ++ show (summaryFailures summary) ++ "\n" ++
      "output of the test-suite:\n" ++
      indent output

indent :: String -> String
indent = unlines . map ("    " ++ ) . lines


data ExpectedEq = ExpectedEq1 | ExpectedEq2 deriving (Generic, Show)

instance Hashable ExpectedEq
instance Arbitrary ExpectedEq where
  arbitrary = oneof [pure ExpectedEq1, pure ExpectedEq2]

-- explicity declare Eq such that every item is equal to itself
instance Eq ExpectedEq where
  ExpectedEq1 == ExpectedEq1 = True
  ExpectedEq2 == ExpectedEq2 = True
  _          == _          = False

data UnexpectedEq = UnexpectedEq1 | UnexpectedEq2 deriving (Generic, Show)

instance Hashable UnexpectedEq
instance Arbitrary UnexpectedEq where
  arbitrary = oneof [pure UnexpectedEq1, pure UnexpectedEq2]

-- explicity declare Eq such that every item is not equal to itself
instance Eq UnexpectedEq where
  UnexpectedEq1 == UnexpectedEq2 = True
  UnexpectedEq2 == UnexpectedEq1 = True
  _          == _          = False


data WellDefinedHash = WellDefinedHash {
  wellDefinedHash1 :: Bool
, wellDefinedHash2 :: Int
, wellDefinedHash3 :: String
} deriving (Eq,Generic,Show,Typeable)

instance Arbitrary WellDefinedHash where
  arbitrary = WellDefinedHash <$> arbitrary <*> arbitrary <*> arbitrary
instance Hashable WellDefinedHash

data PoorlyDefinedHash = PoorlyDefinedHash {
  poorlyDefinedHash1 :: Bool
, poorlyDefinedHash2 :: Int
, poorlyDefinedHash3 :: String
} deriving (Eq,Generic,Show,Typeable)

instance Arbitrary PoorlyDefinedHash where
  arbitrary = PoorlyDefinedHash <$> arbitrary <*> arbitrary <*> arbitrary

-- this creates a hash based on only one selector, it will give incorrect hash values.
instance Hashable PoorlyDefinedHash where
  hashWithSalt s (PoorlyDefinedHash h1 _ _) = hashWithSalt s (h1)

spec :: Spec
spec = do
  describe "testSelfEquality" $ do
    it "pass when Eq on self is defined as expected" $ do
      (s1,_) <- hspecSilently $ testSelfEquality 1000 "ExpectedEq" (Proxy :: Proxy ExpectedEq)
      summaryFailures s1 `shouldBe` 0

    it "fail when Eq on self is not defined as expected" $ do
      (s1,_) <- hspecSilently $ testSelfEquality 1000 "UnexpectedEq" (Proxy :: Proxy UnexpectedEq)
      summaryFailures s1 `shouldBe` 1

  describe "testHashableCollision" $ do
    it "pass when Eq and Hashable are well defined" $ do
      (s1,_) <- hspecSilently $ testHashableCollision 1000 "WellDefinedHash" (Proxy :: Proxy WellDefinedHash)
      summaryFailures s1 `shouldBe` 0
    it "fail when Hashable is poorly defined" $ do
      (s1,_) <- hspecSilently $ testHashableCollision 1000 "PoorlyDefinedHash" (Proxy :: Proxy PoorlyDefinedHash)
      summaryFailures s1 `shouldBe` 1


main :: IO ()
main = hspec spec
