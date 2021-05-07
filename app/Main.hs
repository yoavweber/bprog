module Main where

    import           Control.Monad.State
    import Data.Char (isSpace)
    import Data.List(intercalate)

    import System.Environment

    import qualified Data.Map.Strict     as M
    import           Parser
    import           Stack.Stack
    import           Types
    import Control.Exception
    import System.IO.Error

    data ProgMode = Interpreter | File  | Undefined

    
    -- Interprter excution function
    handleInterpreter :: (AssignmentMap, Stack) -> IO()
    handleInterpreter previousStack = do
            line <- getLine
            let (newVarMap,newStack) = executeCode (unwords $ words line) previousStack
            handleReadStack <- handleRead' newStack
            let (t,newStack) = executeCode "" (newVarMap,handleReadStack)
            let (io,stack) = splitFilter newStack
            mapM_ print io
            print stack
            handleInterpreter (newVarMap,stack)

    -- file exection main function
    handleFile :: String -> IO()
    handleFile line = do
        let (newVarMap,newStack) = executeCode (unwords $ words line) (M.empty :: AssignmentMap, [])
        handleReadStack <- handleRead' newStack
        let (varMap,newStack) = executeCode "" (newVarMap,handleReadStack)
        let (io,stack) = splitFilter newStack
        mapM_ print io
        let (_,newStack) = executeCode "" (varMap,stack)
        print newStack


    -- check if there are any read, if there are then print it
    handleRead' :: Stack  -> IO Stack
    handleRead' [] =
        return []
    handleRead' ((StackIO "read"):xs) = do
        line <- getLine
        fmap (Literal (StackString line):) (handleRead' xs)
    handleRead' (x:xs) =
        fmap (x:) (handleRead' xs)

    -- spliting between the IO and Errors to the rest of the stack    
    splitFilter :: [StackElement] -> ([StackElement], [StackElement])
    splitFilter stack = (filter (`handleIOandErrors` True) stack,filter (`handleIOandErrors` False) stack)
    
    handleIOandErrors :: StackElement -> Bool -> Bool
    handleIOandErrors e bool =
        case e of
            StackIO "read" -> not bool
            StackIO _ -> bool
            Error  _  -> bool
            _         -> not bool

    chooseMode :: IO ProgMode
    chooseMode = do
        putStrLn "Press 1 for an interactive session\nPress 2 for file read mode"
        handleMood <$> getLine

    handleMood :: String -> ProgMode
    handleMood line
        | line == "1" = Interpreter
        | line == "2" = File
        | otherwise = Undefined

    welcomeFunction :: IO ()
    welcomeFunction = do
        mode <- chooseMode
        case mode of
            Undefined -> do 
                 putStrLn "This was not an option, please try again"
                 main
            Interpreter ->
                handleInterpreter (M.empty :: AssignmentMap, [])
            File -> do
                    putStrLn "Please type the name of the file"
                    fileName <- getLine
                    maybeFileContent <- safeRead fileName
                    case maybeFileContent of
                        Just content -> do
                            let parsed = unwords $ words $ unwords $ lines content
                            handleFile parsed
                        Nothing  -> do
                            putStrLn "No such file, please try again"
                            welcomeFunction


    safeRead :: String -> IO (Maybe String)
    safeRead path = fmap Just (readFile path) `catch` handleExists
        where
            handleExists :: IOException -> IO (Maybe String)
            handleExists e
              | isDoesNotExistError e = return Nothing
              | otherwise = throwIO e

    main = do
       putStrLn "\n\nWelcome to bprog\n" 
       welcomeFunction

