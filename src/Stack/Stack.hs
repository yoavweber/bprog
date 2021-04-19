module Stack.Stack (
    stackManip,
    executeCode,
    removeOp
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
        let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ previousStack
        -- let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ tail(previousStack) 
        in execState stackManip (varMap,stack) 

    stackManip ::  ProgState ()
    stackManip = do
        (varMap,currentStack) <- get
        let removeTokens = (filter (\token -> removeOp token) currentStack )
        put (varMap,removeTokens) 
        changeState currentStack
        (_,newStack) <- get
        lastEval newStack
        return ()

    -- evaluating the varibles to there actual values  
    lastEval :: Stack -> ProgState ()
    lastEval [] = return ()
    lastEval (x:xs) = do
         a <- popAndEval
         push a
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
        Literal (Variable var) -> assignVariable (Variable var)
        ControlFlow t -> handleControlFlow t
        otherwise -> return ()
        

    removeOp :: StackElement -> Bool
    removeOp token = case token of
        -- Literal _ -> True
        Literal _ -> True
        Exec _ -> True
        VariableStack _ -> True
        -- Variable _ -> True
        otherwise -> False
    


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
        currentStack <- get
        case (length currentStack) >= 3 of
            True -> do
                condition <- pop
                trueExec <- pop
                falseExec <- pop
            -- if there are no two execution provide an error
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
                put ((M.empty :: AssignmentMap),[Literal (StackString "if input error")]) -- handle error with proper if statment
                return ()
                