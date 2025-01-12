module Main (main) where

import NormalGame (startNormalGame)
import EasyGame (startGameEasyMode)
import IOOperations ( loadDictionary, getIntFromUser, validateIntInputFromUser )
import ExpertGame ( startGameExpertMode )
import HelperMode ( startGameHelperMode ) 
import HelperExpertMode (startGameHelperExpertMode)
import GameState (updateGameStateEasyMode, emptyGameState, GameState (yellowLetters))
import ColorUtils
import System.Random
import Lib

dictName :: String
dictName = "wordle-list.txt"

main :: IO ()
main = do
    putStrLn "---------------------------"
    putStrLn "Добре дошъл в играта WORDLE!"
    putStrLn "Моля, избери какъв режим на игра ще играеш:\n \
                \ 1 -> Режим 'Игра'\n \
                \ 2 -> Режим 'Помощник'"
    getMode <- validateIntInputFromUser "Моля, въведете числото на режима: " 1 2
    if getMode == 1
        then do
            putStrLn "Моля, избери кой режим на игра искаш:\n \
                        \ 1 -> Нормален режим на игра\n \
                        \ 2 -> Лесен режим на игра\n \
                        \ 3 -> Експертен режим на игра"
            getModeGame <- validateIntInputFromUser "Моля, въведете число на режима: " 1 3
            wordLength <- getIntFromUser "Моля, въведете желаната дължина на думите: "
            dict <- loadDictionary dictName wordLength
            indexRandomWord <- randomRIO (0, length dict - 1) :: IO Int
            let secretWord = dict !! indexRandomWord
            case getModeGame of
                1 -> startNormalGame secretWord
                2 -> startGameEasyMode dict secretWord
                3 -> startGameExpertMode secretWord
                _ -> putStrLn "Невлиден режим"
        else do
            putStrLn "Моля, избери кой режим на игра искаш:\n \
                        \ 1 -> Нормален режим на игра\n \
                        \ 2 -> Експертен режим на игра"
            getModeGame <- validateIntInputFromUser "Моля, въведете число на режима: " 1 2
            case getModeGame of
                1 -> startGameHelperMode dictName
                2 -> startGameHelperExpertMode dictName
                _ -> putStrLn "Невaлиден режим"

