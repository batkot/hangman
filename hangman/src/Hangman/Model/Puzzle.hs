{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hangman.Model.Puzzle
    ( PuzzleState(..)
    , Puzzle
    , Solution
    , createPuzzle
    , guessLetter
    , pattern SolvedPuzzle
    , getSolution
    ) where

import Data.List.NonEmpty (NonEmpty)
import GHC.Unicode (toUpper)

data PuzzleLetter
    = Hidden Char
    | Guessed Char
    deriving stock Eq

instance Show PuzzleLetter where
    show (Hidden x) = ['|', x, '|']
    show (Guessed x) = [x]

checkLetter :: Char -> PuzzleLetter -> PuzzleLetter
checkLetter guess (Hidden x)
    | x == guess = Guessed x
    | otherwise = Hidden x
checkLetter _ x = x

letterIsGuessed :: PuzzleLetter -> Bool
letterIsGuessed (Guessed _) = True
letterIsGuessed (Hidden _) = False

letterToChar :: PuzzleLetter -> Char
letterToChar (Hidden x) = x
letterToChar (Guessed x) = x

type Solution = NonEmpty Char

data PuzzleState = Unsolved | Solved

data Puzzle (state :: PuzzleState) where
    MkUnsolvedPuzzle :: NonEmpty PuzzleLetter -> Puzzle 'Unsolved
    MkSolvedPuzzle :: Solution -> Puzzle 'Solved

deriving stock instance Eq (Puzzle state)
deriving stock instance Show (Puzzle state)

pattern SolvedPuzzle :: Solution -> Puzzle 'Solved
pattern SolvedPuzzle solution <- MkSolvedPuzzle solution

getSolution :: Puzzle 'Solved -> Solution
getSolution (MkSolvedPuzzle solution) = solution

createPuzzle :: Solution -> Puzzle 'Unsolved
createPuzzle solution = MkUnsolvedPuzzle $ Hidden . toUpper <$> solution

guessLetter :: Char -> Puzzle 'Unsolved -> Either (Puzzle 'Solved) (Puzzle 'Unsolved)
guessLetter guess (MkUnsolvedPuzzle puzzle)
    | isSolved = Left $ MkSolvedPuzzle solution
    | otherwise = Right $ MkUnsolvedPuzzle newPuzzle
  where
    newPuzzle = checkLetter (toUpper guess) <$> puzzle
    isSolved = all letterIsGuessed newPuzzle
    solution = letterToChar <$> newPuzzle
