module Main (main) where

import NormalGame (startGame)
import EasyGame (startGameEasyMode)
import IOOperations (loadDictonary, getLengthWords)

import System.IO



main :: IO ()
main = do
    -- реално тази дума ще се избира на рандом принцип от речника с думи.
    --let actualWord = "actual"
    --startGame actualWord
    numLength <- getLengthWords 
    contents <- loadDictonary "wordle-list.txt" numLength
    print contents
    let actualWord = "apple"
    startGameEasyMode actualWord contents