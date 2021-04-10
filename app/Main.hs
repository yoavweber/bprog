module Main where

import Control.Monad.State
import Parser
import Types
import Stack.Stack

    
updateState :: Stack -> IO()
updateState previousStack = do
    line <- getLine
    let tokenize = map (\e -> getTokeType e) $ words line
    let stack =  tokenize ++ previousStack
    let y = (execState stackManip) stack
    print $ y
    updateState y



main = do
    updateState []



