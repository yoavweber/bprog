module Stack.Stack (
    stackManip,
    executeCode,
    removeOp,
    changeState
 ) where
    import Control.Monad.State
    import Types
    import qualified Data.Map.Strict as M


    import Operations.StackOp
    import Operations.Arithmetic
    import Operations.ListOp
    import Operations.Symbol
    import Stack.StackOperations
    import Parser



    executeCode :: String -> (AssignmentMap, Stack) -> (AssignmentMap, Stack)
    executeCode line (varMap,previousStack) = 
        let stack = previousStack ++ (map (\e -> getTokenType e) $ tokenize (words line))
        in execState stackManip (varMap,stack) 

    stackManip ::  ProgState ()
    stackManip = do
        -- temp stack, just a way to pass the tokens
        (varMap,currentStack) <- get
        -- FIXME: this is a fix, refactoring is needed
        put (varMap,[])
        -- let firstEval = (filter (\token -> evalValues token) currentStack )
        -- handling simple operations that don't require look up
        -- changeState firstEval
        changeState currentStack


        -- this is with all the complex operations
        -- let sndEval = (filter (\token -> removeOp token) currentStack )
        -- changeState sndEval

        -- put (varMap,removeTokens)
        -- changeState newStack
        (varMap,newStacks) <- get 
        -- evaluating the stack, and transforming the varilbes to their values
        -- let evalStack = eval newStack varMap

        -- let evalStack = map (\e -> eval e varMap) newStack
        -- put (varMap,newStacks)
        return ()


    changeState :: Stack -> ProgState ()
    changeState [] = return () 
    changeState (x:xs) = do
        handleTokens x
        changeState xs


    handleTokens :: StackElement -> ProgState ()
    handleTokens t = case t of
        Arithmetic t ->  handleAritmic  (Arithmetic t)
        StackOp t -> handleStackOp (StackOp t)
        ListOp t -> handleListOp t
        AssignmentOp t -> handleVariable t
        ControlFlow t -> handleControlFlow t
        Literal (Variable var) -> assignVariable (Variable var)
        Literal var -> pushToEnd (Literal var)
        otherwise -> return ()
        

      -- evaluating the varibles to there actual values  
    eval ::  StackElement -> AssignmentMap -> StackElement
    eval maybeVar assignmentMap = case maybeVar of
        Literal (Variable var) -> case M.lookup (Literal (Variable var)) assignmentMap of
            Nothing ->  (Literal $ StackString "Error") -- TODO: error, there is no such variable(this is a problem in the program)
            Just n -> Literal n
        
        otherwise -> maybeVar 


-- ---------------------- Control flow --------------------------------

    handleControlFlow :: String -> ProgState ()
    handleControlFlow t = case t of
        "exec" -> handleExecution 
        "if" -> handleIf
    

    executeCodeLine :: String -> (AssignmentMap, Stack) -> StackElement
    executeCodeLine line (varMap,previousStack) = 
        let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ previousStack
        in snd (execState stackManip (varMap, stack)) !! 0
    


    unWrap :: Ops -> String
    unWrap a = case a of
        -- Literal t -> t 
        Exec t -> t

    handleExecution :: ProgState ()
    handleExecution = do
        executionLine <- pop
        let command = unWrap executionLine
        let res = executeCodeLine command ((M.empty :: AssignmentMap), [])
        push (res)
        return ()


    handleIf :: ProgState ()
    handleIf = do
        (_,currentStack) <- get
        case (length currentStack) >= 3 of
            True -> do
                condition <- popFromEnd
                trueExec <- popFromEnd
                falseExec <- popFromEnd
            -- TODO: error, if there are no two execution provide an error
                case condition of
                    Literal (StackBool True)  -> do
                        push (executeCodeLine (unWrap trueExec) ((M.empty :: AssignmentMap), []))
                        return ()
                    Literal (StackBool False) -> do
                        push (executeCodeLine (unWrap falseExec) ((M.empty :: AssignmentMap), []))
                        return ()
                    otherwise -> do
                        push (Literal (StackString "if input error, you didn't provided bool condition")) -- handle error
                        return ()
            False -> do 
                put ((M.empty :: AssignmentMap),[Literal (StackString $ "if input error, not enough arguments: " ++ (show $ length currentStack) )]) -- handle error with proper if statment
                return ()
                

    -- deprecated
    removeOp :: StackElement -> Bool
    removeOp token = case token of
        Literal _ -> True
        Exec _ -> True
        otherwise -> False