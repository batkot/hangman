{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hangman.PositiveInt
    ( createPositiveInt
    , PositiveInt
    , pattern PositiveInt

    , one
    , increment
    , decrement
    , add
    , toInt
    ) where
import Data.Either.Combinators (rightToMaybe)

newtype PositiveInt =
    MkPositiveInt { unPositiveInt :: Int }
    deriving stock Eq
    deriving newtype (Show, Ord)

pattern PositiveInt :: Int -> PositiveInt
pattern PositiveInt x <- MkPositiveInt x

createPositiveInt :: Int -> Either String PositiveInt
createPositiveInt x
    | x > 0 = Right $ MkPositiveInt x
    | otherwise = Left $ show x <> " is not positive int"

one :: PositiveInt
one = MkPositiveInt 1

add :: PositiveInt -> PositiveInt -> PositiveInt
add (MkPositiveInt x) (MkPositiveInt y) = MkPositiveInt $ x + y

increment :: PositiveInt -> PositiveInt
increment = add one

decrement :: PositiveInt -> Maybe PositiveInt
decrement (MkPositiveInt x) = rightToMaybe . createPositiveInt $ x - 1

toInt :: PositiveInt -> Int
toInt = unPositiveInt
