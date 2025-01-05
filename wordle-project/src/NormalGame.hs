module NormalGame ( startGame, evaluateGuess, changeColor, colorTheLetters ) where
import System.IO ( hFlush, stdout )
import HelperMode ( getColorMatching )
import Data.List (sortBy)
import Data.Ord (comparing)

-- да направя ли да може да се exit-ва от играта, ако напише exit
startGame :: String -> IO ()
startGame actualWord = do
    guess <- evaluateGuess actualWord (length actualWord)
    if actualWord == guess
        then do
            putStrLn ("\x1b[32mБраво! Позна думата " ++ actualWord ++ "!\x1b[0m")
        else
            startGame actualWord 

evaluateGuess :: String -> Int -> IO String
evaluateGuess actualWord wordLength = do
    putStr "Въведи опит за познаване на думата: "
    hFlush stdout
    guess <- getLine
    if length guess /= wordLength
        then do
            putStrLn ("\x1b[31mДумата трябва да бъде с дължина " ++ show wordLength ++ "\x1b[0m")
            evaluateGuess actualWord wordLength
            -- TO DO: To clear the screen after a difference in the length
        else do
            return (colorTheLetters guess actualWord)
            

colorTheLetters guess actualWord = 
    let colorPattern = sortBy (comparing  (\(i, _, _) -> i)) (getColorMatching guess actualWord)
    in concatMap (\(_, letter, color) -> changeColor color letter) colorPattern

-- правя низът в един цял стринг "\x1b[32m[letter]\x1b[0m\x1b[32m"[letter]\x1b[0m\x1b[32m[letter]\x1b[0m"
-- от този тип и в горната функция го печатам
-- colorTheLetters :: [Char] -> [Char] -> [Char]
-- colorTheLetters guess actualWord = helper guess actualWord "" actualWord where
--     helper [] [] result _ = result -- two of the lists are empty -> return result
--     helper [] _ result _ = result
--     helper _ [] result _ = result
--     helper (x:xs) (y:ys) result contains
--         | x == y        = helper xs ys (result ++ changeColor "green" x) (removeOccuranceFromActualWord contains x)
--         | x `elem` contains   = helper xs ys (result ++ changeColor "yellow" x) (removeOccuranceFromActualWord contains x)
--         | otherwise     = helper xs ys (result ++ changeColor "gray" x) contains

-- removeOccuranceFromActualWord :: Eq t => [t] -> t -> [t]
-- removeOccuranceFromActualWord [] _ = []
-- removeOccuranceFromActualWord (x:xs) letter = if x == letter then xs else x : removeOccuranceFromActualWord xs letter

-- to say that it is from chatGpt
changeColor :: String -> Char -> [Char]
changeColor wantedColor letter
    | wantedColor == "gr"     = "\x1b[32m" ++ [letter] ++ "\x1b[0m"
    | wantedColor == "y"    = "\x1b[33m" ++ [letter] ++ "\x1b[0m"
    | otherwise                  = "\x1b[90m" ++ [letter] ++ "\x1b[0m"
