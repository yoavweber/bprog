module Main where

import Control.Monad.State
import Parser
import Types
import Stack.Stack

    
updateState :: Stack -> IO()
updateState previousStack = do
    line <- getLine
    let rawList = words line
    let tokenizeList = map (\e -> getTokeType e) $ tokenize rawList
    print $ (tokenizeList, "Tokenize List") 
    let stack =  tokenizeList ++ previousStack

    print $ (stack, "stack before manipulation")
    let y = (execState stackManip) stack
    print $ (y, "stack after manipulation")
    print $ (y !! 0, "stack top value")
    updateState y



main = do
    updateState []



