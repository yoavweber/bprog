module Main where

import Control.Monad.State
import Parser
import Types
import Stack.Stack
import qualified Data.Map.Strict as M


updateState :: (AssignmentMap, Stack) -> IO()
updateState previousStack = do
    -- print $ (previousStack, "Previoues stack") 
    line <- getLine
    let (newVarMap,newStack) = executeCode line previousStack
    print $ (newStack, "stack after manipulation")
    -- print $ ( newStack !! 0, "stack top value")
    print $ (newVarMap, "var map after manipulation")
    updateState (newVarMap,newStack)



main = do
    updateState ((M.empty :: AssignmentMap), [])



