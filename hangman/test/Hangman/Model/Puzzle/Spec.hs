{-# LANGUAGE RecordWildCards #-}

module Hangman.Model.Puzzle.Spec
    ( test_puzzle
    ) where

import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.QuickCheck          (InfiniteList (getInfiniteList),
                                                 testProperty)

import           Data.Bifunctor                 (first)
import           Data.Foldable                  (foldrM)
import qualified Data.List.NonEmpty             as NonEmpty (head)
import           Data.List.NonEmpty.Extra       (nubOrdOn)
import           GHC.Unicode                    (toLower, toUpper)
import           Hangman.Model.Puzzle
import           Hangman.Model.Puzzle.Arbitrary

test_puzzle :: TestTree
test_puzzle = testGroup "Hangman.Model.Puzzle tests"
    [ testProperty "Guessing wrong letter does not change the puzzle" guessingWrongLetterDoesntChangePuzzle
    , testProperty "Guessing same letter twice does not change the puzzle" guessingSameLetterTwiceDoesntChangePuzzle
    , testProperty "Guessing all solution letters solves the puzzle" guessingAllSolutionLettersSolvesThePuzzle
    , testProperty "Guessing letter should be case-insensitive" guessingLetterIsCaseInsensitive
    , testGroup "Unsolved puzzle tests"
        [ testProperty "Guessed letter is visible in description" guessedLetterIsVisibleInDescription
        , testProperty "Fresh puzzle should have all chars hidden" freshPuzzleShouldHaveAllCharsHidden
        ]
    ]

guessingWrongLetterDoesntChangePuzzle :: UnsolvedPuzzle -> InfiniteList Char -> Bool
guessingWrongLetterDoesntChangePuzzle UnsolvedPuzzle{..} infiniteChars =
    Right puzzle == guessLetter wrongGuess puzzle
  where
    puzzleChars = toUpper <$> nubOrdOn toUpper solution
    wrongGuess = head . filter ((`notElem` puzzleChars) . toUpper) . getInfiniteList $ infiniteChars

guessingSameLetterTwiceDoesntChangePuzzle :: UnsolvedPuzzle -> Char -> Bool
guessingSameLetterTwiceDoesntChangePuzzle UnsolvedPuzzle{..} guess =
    singleGuess == secondGuess
  where
    singleGuess = guessLetter guess puzzle
    secondGuess = singleGuess >>= guessLetter guess

guessingAllSolutionLettersSolvesThePuzzle :: UnsolvedPuzzle -> Bool
guessingAllSolutionLettersSolvesThePuzzle UnsolvedPuzzle{..} =
    Left (toUpper <$> solution) == guessResult
  where
    guessResult = first getSolution $ foldrM guessLetter puzzle solution

guessingLetterIsCaseInsensitive :: UnsolvedPuzzle -> Char -> Bool
guessingLetterIsCaseInsensitive UnsolvedPuzzle{..} guess =
    guessLetter (toUpper guess) puzzle == guessLetter (toLower guess) puzzle

guessedLetterIsVisibleInDescription :: UnsolvedPuzzle -> Bool
guessedLetterIsVisibleInDescription UnsolvedPuzzle{..} =
    elem (Just guess) . either (fmap Just . getSolution) describePuzzle $ guessLetter guess puzzle
  where
    guess = toUpper $ NonEmpty.head solution

freshPuzzleShouldHaveAllCharsHidden :: UnsolvedPuzzle -> Bool
freshPuzzleShouldHaveAllCharsHidden UnsolvedPuzzle{..} = all (Nothing ==) $ describePuzzle puzzle
