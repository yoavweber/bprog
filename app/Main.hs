module Main where

    import           Control.Monad.State
    import qualified Data.Map.Strict     as M
    import           Parser
    import           Stack.Stack
    import           Types

    updateState :: (AssignmentMap, Stack) -> IO()
    updateState previousStack = do
        -- print $ (previousStack, "Previoues stack")
        line <- getLine
        prasingRead <- handleRead' $ words line
        let (newVarMap,newStack) = executeCode (unwords prasingRead) previousStack

        let (io,stack) = splitFilter newStack
        mapM_ print io
        print (stack, "stack after manipulation")
        print (newVarMap, "var map after manipulation")
        updateState (newVarMap,stack)


    handleRead' :: [String] -> IO[String]
    handleRead' [] =  do
        return []
    handleRead' ("read":xs) = do
        line <- getLine
        fmap (("\"" ++ line ++ "\""):) (handleRead' xs)
    handleRead' (x:xs) = do
        fmap (x:) (handleRead' xs)

    splitFilter stack = (filter (`handleIOandErrors` True) stack,filter (`handleIOandErrors` False) stack)

    handleIOandErrors :: StackElement -> Bool -> Bool
    handleIOandErrors e bool =
        case e of
            StackIO _ -> bool
            Error  _  -> bool
            _         -> not bool


    main = do
        updateState (M.empty :: AssignmentMap, [])
