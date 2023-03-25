{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hangman.Model.Puzzle
    ( PuzzleState(..)
    , Puzzle
    , Solution
    , createPuzzle
    , guessLetter
    , getSolution
    , describePuzzle
    ) where

import           Data.List.NonEmpty (NonEmpty)
import           GHC.Unicode        (toUpper)

data PuzzleLetter
    = Hidden Char
    | Guessed Char
    deriving stock Eq

instance Show PuzzleLetter where
    show (Hidden x)  = ['|', x, '|']
    show (Guessed x) = [x]

checkLetter :: Char -> PuzzleLetter -> PuzzleLetter
checkLetter guess (Hidden x)
    | x == guess = Guessed x
    | otherwise = Hidden x
checkLetter _ x = x

letterIsGuessed :: PuzzleLetter -> Bool
letterIsGuessed (Guessed _) = True
letterIsGuessed (Hidden _)  = False

letterToChar :: PuzzleLetter -> Char
letterToChar (Hidden x)  = x
letterToChar (Guessed x) = x

letterToMaybe :: PuzzleLetter -> Maybe Char
letterToMaybe (Guessed x) = Just x
letterToMaybe _           = Nothing

type Solution = NonEmpty Char

data PuzzleState = Unsolved | Solved

data Puzzle (state :: PuzzleState) where
    UnsolvedPuzzle :: NonEmpty PuzzleLetter -> Puzzle 'Unsolved
    SolvedPuzzle :: Solution -> Puzzle 'Solved

deriving stock instance Eq (Puzzle state)
deriving stock instance Show (Puzzle state)

getSolution :: Puzzle state -> Solution
getSolution (SolvedPuzzle solution) = solution
getSolution (UnsolvedPuzzle puzzle) = letterToChar <$> puzzle

type PuzzleDescription = NonEmpty (Maybe Char)

describePuzzle :: Puzzle 'Unsolved -> PuzzleDescription
describePuzzle (UnsolvedPuzzle puzzle) = letterToMaybe <$> puzzle

createPuzzle :: Solution -> Puzzle 'Unsolved
createPuzzle solution = UnsolvedPuzzle $ Hidden . toUpper <$> solution

guessLetter :: Char -> Puzzle 'Unsolved -> Either (Puzzle 'Solved) (Puzzle 'Unsolved)
guessLetter guess (UnsolvedPuzzle puzzle)
    | isSolved = Left $ SolvedPuzzle solution
    | otherwise = Right $ UnsolvedPuzzle newPuzzle
  where
    newPuzzle = checkLetter (toUpper guess) <$> puzzle
    isSolved = all letterIsGuessed newPuzzle
    solution = letterToChar <$> newPuzzle
