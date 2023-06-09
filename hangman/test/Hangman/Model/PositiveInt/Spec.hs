module Hangman.Model.PositiveInt.Spec
    ( test_positiveInt
    ) where

import           Data.Either                         (isRight)
import           Hangman.Model.PositiveInt
import           Hangman.Model.PositiveInt.Arbitrary ()
import           Test.HUnit                          ((@?=))
import           Test.Tasty                          (TestTree, testGroup)
import           Test.Tasty.HUnit                    (testCase)
import           Test.Tasty.QuickCheck               (testProperty)

test_positiveInt :: TestTree
test_positiveInt =
    testGroup "Hangman.Model.PositiveInt tests"
        [ testProperty "PositiveInt is always greater than 0" positiveIntIsAlwaysGreaterThanZero
        , testProperty "Adding PositiveInts is isomorphic to multiple increments" addPositiveIntegersIsMultipleIncrement
        , testProperty "Obligatory addition commutative property" addIsCommutative
        , testProperty "Incremented PositiveInt is greater" incrementIncreasesPositiveInt
        , testProperty "Increment and decrement should cancel out" incrementAndDecrementCancelOut
        , testProperty "PositiveInt should be recreatable" positiveIntShouldBeRecreatable
        , testProperty "|x| + 1 - is positive int" absPlusOneIsAlwaysPositiveInt
        , testProperty "Negative of PositiveInt is not a PositiveInt" negativeOfPositiveIntIsNotPositiveInt
        , testCase "Zero is not PositiveInt" (createPositiveInt 0 @?= Left "0 is not positive int")
        , testCase "Can't decrement one" (decrement one @?= Nothing)
        ]

positiveIntIsAlwaysGreaterThanZero :: PositiveInt -> Bool
positiveIntIsAlwaysGreaterThanZero = (<) 0 . toInt

addPositiveIntegersIsMultipleIncrement :: PositiveInt -> PositiveInt -> Bool
addPositiveIntegersIsMultipleIncrement x y = addResult == incrementResult
  where
    addResult = add x y
    incrementResult = iterate increment x !! toInt y

addIsCommutative :: PositiveInt -> PositiveInt -> Bool
addIsCommutative x y = add x y == add y x

incrementIncreasesPositiveInt :: PositiveInt -> Bool
incrementIncreasesPositiveInt x = increment x > x

positiveIntShouldBeRecreatable :: PositiveInt -> Bool
positiveIntShouldBeRecreatable x = Right x == (createPositiveInt . toInt $ x)

negativeOfPositiveIntIsNotPositiveInt :: PositiveInt -> Bool
negativeOfPositiveIntIsNotPositiveInt x = Left ("-" <> show x <> " is not positive int") == createPositiveInt negative
  where
    negative = negate . toInt $ x

absPlusOneIsAlwaysPositiveInt :: Int -> Bool
absPlusOneIsAlwaysPositiveInt = isRight . createPositiveInt . (+1) . abs

incrementAndDecrementCancelOut :: PositiveInt -> Bool
incrementAndDecrementCancelOut x = Just x == (decrement . increment $ x)
