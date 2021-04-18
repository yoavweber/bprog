module Main where

import Control.Monad.State
import Parser
import Types
import Stack.Stack
import qualified Data.Map.Strict as M


updateState :: Stack -> IO()
updateState previousStack = do
    -- print $ (previousStack, "Previoues stack") 
    line <- getLine
    let newStack = executeCode line previousStack
    print $ (newStack, "stack after manipulation")
    print $ ( newStack !! 0, "stack top value")
    updateState (newStack)



main = do
    updateState [VaribleStack $ M.fromList[(Literal (Varible "Test"), StackString "test")]]

    -- updateState [VaribleStack $ M.fromList[("Test", StackString "test")]]



