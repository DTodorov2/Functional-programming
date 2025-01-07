module GameState ( getFiltersCount, updateGameState, GameState(greenLetters, yellowLetters, grayLetters), getAllFilters, emptyGameState) where

import Data.List (nub)
import Color ( Color(..) )
import ColorUtils ( getColorMatching )

type GrayLetters = [(Int, Char, Color)]
type YellowLetters = [(Int, Char, Color)]
type GreenLetters = [(Int, Char, Color)]

data GameState = GameState {
    grayLetters :: GrayLetters,
    yellowLetters :: YellowLetters,
    greenLetters :: GreenLetters
    }

getAllFilters :: GameState -> [(Int, Char, Color)]
getAllFilters gameState =
    grayLetters gameState ++
    yellowLetters gameState ++
    greenLetters gameState

emptyGameState :: GameState
emptyGameState = GameState [] [] []

filterLettersByColor :: Eq a1 => a1 -> [(a2, b, a1)] -> [(a2, b, a1)]
filterLettersByColor color = filter (\(_, _, c) -> c == color)

newGrayLettersList :: [(a2, b, Color)] -> [(a2, b, Color)]
newGrayLettersList = filterLettersByColor Gray

newYellowLettersList :: [(a2, b, Color)] -> [(a2, b, Color)]
newYellowLettersList = filterLettersByColor Yellow

newGreenLettersList :: [(a2, b, Color)] -> [(a2, b, Color)]
newGreenLettersList =  filterLettersByColor Green

updateGameState :: [Char] -> [Char] -> GameState -> GameState
updateGameState guess actualWord gameState =
    let colorMatchingPattern = getColorMatching guess actualWord
    in GameState {
        grayLetters = nub (grayLetters gameState ++ newGrayLettersList colorMatchingPattern),
        yellowLetters = nub (yellowLetters gameState ++ newYellowLettersList colorMatchingPattern),
        greenLetters = nub (greenLetters gameState ++ newGreenLettersList colorMatchingPattern)
        }

getFiltersCount gameState =
    length (nub [ letter | (_, letter, _) <- yellowLetters gameState ++ greenLetters gameState])
