module ExpertGame (startGameExpertMode) where

import NormalGame
    ( colorTheLetters, removeOccuranceFromActualWord, changeColor )
import System.IO ( hFlush, stdout )
import EasyGame
    ( GameState(usedGrayLetters, usedGreenLetters, usedYellowLetters),
      emptyGameState,
      updateGameState )

startGameExpertMode :: String -> IO ()
startGameExpertMode actualWord = evaluateGuessExpertGame actualWord (length actualWord) emptyGameState False

evaluateGuessExpertGame :: String -> Int -> GameState -> Bool -> IO ()
evaluateGuessExpertGame actualWord wordLength gameState usedLie = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ("\x1b[31mДумата трябва да бъде с дължина " ++ show wordLength ++ "\x1b[0m")
            evaluateGuessExpertGame actualWord wordLength gameState usedLie
            -- TO DO: To clear the screen after a difference in the length
            else if guess == actualWord
                then do
                    putStrLn ("\x1b[32mБраво! Позна думата " ++ actualWord ++ "!\x1b[0m")
                else do
                    -- shouldLie <- randomRIO (0, 1 :: Int)
                    let shouldLie = True
                    putStrLn (colorTheLettersExpertMode guess actualWord shouldLie gameState usedLie)
                    evaluateGuessExpertGame actualWord wordLength (updateGameState guess actualWord gameState) (if usedLie then True else shouldLie)


colorTheLettersExpertMode :: [Char] -> [Char] -> Bool -> GameState -> Bool -> [Char]
colorTheLettersExpertMode guess actualWord isLie gameState usedLie =
    -- To be careful with the conditions in the if
    if usedLie || not isLie
        then colorTheLetters guess actualWord
        else colorTheLettersFalsly guess actualWord gameState

colorTheLettersFalsly :: [Char] -> [Char] -> GameState -> [Char]
colorTheLettersFalsly guess actualWord gameState = helper guess actualWord "" actualWord where
    helper [] [] result _ = result -- two of the lists are empty -> return result
    helper [] _ result _ = result
    helper _ [] result _ = result
    helper (x:xs) (y:ys) result contains
        | x == y                = helper xs ys (result ++ colorForGreen x gameState) (removeOccuranceFromActualWord contains x)
        | x `elem` contains     = helper xs ys (result ++ colorForYellow x gameState) (removeOccuranceFromActualWord contains x)
        | otherwise             = helper xs ys (result ++ colorForGray x gameState) contains

colorForGreen :: Char -> GameState -> [Char]
colorForGreen letter gameState
    | letter `elem` map snd (usedGreenLetters gameState) = changeColor "green" letter
    | otherwise                                          = changeColor "yellow" letter

colorForYellow :: Char -> GameState -> [Char]
colorForYellow letter gameState
    | letter `elem` usedYellowLetters gameState = changeColor "yellow" letter
    | otherwise                                 = changeColor "gray" letter

colorForGray :: Char -> GameState -> [Char]
colorForGray letter gameState
    | letter `elem` usedGrayLetters gameState = changeColor "gray" letter
    | otherwise                               = changeColor "yellow" letter