module Test.PlutusInboxNfts.Unit (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.Prelude
import Plutus.V1.Ledger.Value (TokenName, Value, singleton)

import PlutusInboxNfts.Utils

import Test.PlutusInboxNfts.Unit.Validator qualified as Validator
import Utils

tests :: TestTree
tests = do
  let ?tester = standardTester
  testGroup
    "Unit tests"
    [ pvalueOfTests
    , passertTests
    , isSubsetOfTests
    , Validator.tests
    ]

pvalueOfTests :: HasTester => TestTree
pvalueOfTests = do
  let testValue = pconstant $ singleton "a1" "b1" 1
        <> singleton "a2" "b2" 2
  testGroup
    "pvalueOf tests"
    [ testCase "pvalueOf absent asset class" $ do
        expect $ pvalueOf # testValue # pconstant "a0" # pconstant "b0" #== 0
    , testCase "pvalueOf present asset class" $ do
        expect $ pvalueOf # testValue # pconstant "a2" # pconstant "b2" #== 2
    ]

passertTests :: HasTester => TestTree
passertTests = do
  testGroup
    "passert tests"
    [ testCase "passert succeeds" $ do
        expect $ passert (pconstant True) (pconstant ()) #== pconstant ()
    , testCase "passert fails" $ do
        fails $ passert (pconstant False) (pconstant ())
    ]

-- | Construct a value of n coins of some currency identified by token name; just for brevity
coins :: TokenName -> Integer -> Value
coins = singleton "c3"

isSubsetOfTests :: HasTester => TestTree
isSubsetOfTests = do
  testGroup
    "isSubsetOf tests"
    [ testCase "proper subset" $ do
        expect $ isSubsetOf
          # (pconstant $ coins "A" 1 <> coins "B" 2)
          # (pconstant $ coins "B" 2 <> coins "A" 3 <> coins "C" 5)
    , testCase "same set is subset" $ do
        expect $ isSubsetOf
          # (pconstant $ coins "A" 1 <> coins "B" 2)
          # (pconstant $ coins "B" 2 <> coins "A" 3)
    , testCase "not subset" $ do
        expect $ pnot #$ isSubsetOf
          # (pconstant $ coins "A" 1 <> coins "B" 2 <> coins "C" 5)
          # (pconstant $ coins "B" 2 <> coins "A" 3)
    , testCase "edge case: 0 in the subset" $ do
        expect $ isSubsetOf
          # (pconstant $ coins "A" 1 <> coins "B" 2 <> coins "C" 0)
          # (pconstant $ coins "B" 2 <> coins "A" 3)
    , testCase "edge case: 0 in the superset" $ do
        expect $ pnot #$ isSubsetOf
          # (pconstant $ coins "A" 1 <> coins "B" 2)
          # (pconstant $ coins "B" 2 <> coins "A" 0)
    ]
