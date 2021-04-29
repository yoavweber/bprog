module Stack.Stack 
-- (
--     stackManip,
--     executeCode,
--     executeCodeLine,
--     changeState,
--     unWrap,
--     fold',
--     parseLine

--  ) where
    where
    import Control.Monad.State
    import Types
    import qualified Data.Map.Strict as M


    import Operations.StackOp
    import Operations.Arithmetic
    import Operations.ListOp(handleListOp)
    import Operations.Symbol
    -- import Operations.ControlFlow
    import Stack.StateOps
    import Parser



    executeCode :: String -> (AssignmentMap, Stack) -> (AssignmentMap, Stack)
    executeCode line (varMap,previousStack) = 
        let stack = previousStack ++  parseLine line
        in execState stackManip (varMap,stack) 

    stackManip ::  ProgState ()
    stackManip = do
        -- temp stack, just a way to pass the tokens
        ergonomicManip

        (varMap,currentStack) <- get
        put (varMap,[])
        changeState currentStack


        return ()


    changeState :: Stack -> ProgState ()
    changeState [] = return () 
    changeState (x:xs) = do
        handleTokens x
        changeState xs

    -- creating a new stack which would containt the "fixed" ergonomic expressions
    ergonomicManip :: ProgState ()
    ergonomicManip = do
        (varMap,currentStack) <- get
        put (varMap,[])
        loopErgonomic currentStack
        return () 


    loopErgonomic :: Stack -> ProgState ()
    loopErgonomic [] = return ()
    loopErgonomic (x:xs) = do
        handleErgonomic x
        loopErgonomic xs

    -- checking if the element is of ControlFlow type, if it is handling with it
    handleErgonomic :: StackElement  -> ProgState ()
    handleErgonomic e = do
        case e of
            ControlFlow "if" -> do
                ergonomicExpr True
                push $ ControlFlow "if"
                return ()
            ControlFlow controlFlow -> do
                ergonomicExpr False
                push $ ControlFlow controlFlow
                return()
            _ -> do
                push e
                return ()


    -- Removing the argument and replacing it with an expression
    ergonomicExpr :: Bool -> ProgState()
    ergonomicExpr isIf = do
        if isIf
            then do
                exper1 <- wrapExpr
                exper2 <- wrapExpr
                push exper2
                push exper1
                return ()
            else do
                exper <- wrapExpr
                push exper
                return ()
                
    -- check if the control flow recived expresions
    -- if it has not, turn them into expressions
    wrapExpr = do
        -- TODO: error, check stack size
        maybeExpr <- pop
        case maybeExpr of
            Exec _ -> do
                return maybeExpr
            _ -> do
                -- push $ Exec [maybeExper2]
                return $ Exec [maybeExpr]

    handleTokens :: StackElement -> ProgState ()
    handleTokens t = case t of
        Arithmetic t ->  handleAritmic  (Arithmetic t)
        StackOp t -> handleStackOp (StackOp t)
        ListOp t -> handleListOp t
        StackIO _ -> handleIO
        AssignmentOp t -> handleVariable t
        ControlFlow t -> handleControlFlow t
        Exec t -> push (Exec t)
        Literal (Variable var) -> assignVariable (Variable var)
        Literal var -> push (Literal var)
        otherwise -> return ()
        

    handleIO :: ProgState ()
    handleIO = do
        printElement <- pop
        case printElement of
            -- TODO: handle varibles as well
            Literal (StackString e) -> do
                push (StackIO e)
                return ()
            _ -> do
                -- TODO: error, 
                push (Error "can't print a nonstring element")
                return ()
        return ()

      -- evaluating the varibles to there actual values  
    -- eval ::  [StackElement] -> AssignmentMap -> [StackElement]
    -- eval [] _ = []
    -- eval (maybeVar:stack) assignmentMap = case maybeVar of
    --     Literal (Variable var) -> case M.lookup (Literal (Variable var)) assignmentMap of
    --         Nothing ->  (Literal $ StackString "Error") : (eval stack assignmentMap)
    --         Just n -> Literal n:(eval stack assignmentMap)
    --     Literal (List var) ->  eval (map (\e -> Literal e) var) assignmentMap ++
           
    --     -- ControlFlow "if" -> case length stack < 2 of
    --     --     False -> do 
    --     --         execTrue:execFalse:stack
    --     --         (handleIf' execTrue execFalse):(eval stack assignmentMap)
    --         -- True -> do TODO: error, send error and stop the program

    --     otherwise -> maybeVar:(eval stack assignmentMap)
    -- TODO: fix the issue with the order and with the list
    -- eval ::  StackLiteral -> AssignmentMap -> StackLiteral
    -- eval element assignmentMap =  case element of
    --     List list -> List $ map (\e -> StackBool True ) list 
    --     Variable e -> case M.lookup (Literal $ Variable e) assignmentMap of
    --         Nothing ->  StackString "Error"
    --         -- Just (Exec exec) -> (Exec exec)
    --         Just n -> n
    --     _ -> element

    -- evalStack :: Stack -> AssignmentMap -> Stack 
    -- evalStack [] _ = []
    -- evalStack (maybeVar:stack) assignmentMap = case maybeVar of
    --     Literal lit ->  (stack ++ [Literal $ eval lit assignmentMap]) 
    --     _  -> maybeVar:stack
           

-- ---------------------- Control flow --------------------------------
    -- executeCodeLine :: String -> (AssignmentMap, Stack) -> Stack
    -- executeCodeLine line (varMap,previousStack) = 
    --     let stack =  previousStack ++ (parseLine line)
    --     in snd (execState stackManip (varMap, stack))
    -- TODO: change it to executeParsedCode
    executeCodeLine :: Stack -> (AssignmentMap, Stack) -> Stack
    executeCodeLine stack (varMap,previousStack) = 
        let newStack =  previousStack ++ stack
        in snd (execState stackManip (varMap, newStack))

    -- unwrap exec
    unWrap :: Ops -> Stack
    unWrap a = case a of
        -- Literal t -> t 
        Exec t -> t
        -- otherwise ->

    unWrapStackLiteral :: StackElement -> StackLiteral
    unWrapStackLiteral a = case a of
        Literal t -> t 
        -- Exec t -> t
        -- otherwise ->

    handleControlFlow :: String -> ProgState ()
    handleControlFlow t = case t of
        "exec" -> handleExecution 
        "if" -> handleIf
        "map" -> handleMap
        "foldl" -> handleFold
        "times" -> handleTimes
    


    handleExecution :: ProgState ()
    handleExecution = do
        executionLine <- pop
        let command = unWrap executionLine
        let res = executeCodeLine command (M.empty :: AssignmentMap, [])
        concatState res
        return ()


    assignVariable :: StackLiteral -> ProgState ()
    assignVariable (Variable var) = do
        push $ Literal $ Variable var
        assignmentMap <- getVarMap 
        case M.lookup (Variable var) assignmentMap of
            Nothing -> do
                -- let t = M.insert var (Variable "undefined element") assignmentMap
                let t = M.insert (Variable var) ( Literal $ StackString var) assignmentMap
                updateVar t
                return ()
            Just n -> do
                -- poping the variable and inserting the value of it
                pop
                case n of
                    Exec stack -> do
                        (varMap,currentStack) <- get
                        let executedFunc = executeCodeLine (currentStack ++ stack) (varMap, [])
                        put(varMap,executedFunc)
                        return ()
                    Literal s-> do
                        push n
                        return ()


    handleIf :: ProgState ()
    handleIf = do
        (_,currentStack) <- get
        case (length currentStack) >= 3 of
            True -> do
                falseExec <- pop
                trueExec <- pop
                condition <- pop
            -- TODO: error, if there are no two execution provide an error
                case condition of
                    Literal (StackBool True)  -> do
                        concatState (executeCodeLine (unWrap trueExec) ((M.empty :: AssignmentMap), []))
                        stackManip
                        return ()
                    Literal (StackBool False) -> do
                        concatState (executeCodeLine (unWrap falseExec) ((M.empty :: AssignmentMap), []))
                        stackManip
                        return ()
                    otherwise -> do
                        push condition
                        push (Literal (StackString "if input error, you didn't provided bool condition")) -- handle error
                        return ()
            False -> do 
                -- put ((M.empty :: AssignmentMap),[Literal (StackString $ "if input error, not enough arguments: " ++ (show $ length currentStack) )]) -- handle error with proper if statment
                push (ControlFlow "if")
                return ()

    handleEach ::  ProgState ()
    handleEach = do
        experssion <- pop
        list <- pop
        case list of
            Literal (List x) -> do 
                let stackElements = map (\e ->  head $ executeCodeLine (unWrap experssion) ( (M.empty :: AssignmentMap),([Literal e]) ) )  x
                concatState stackElements
        -- currentStack <- get
                return ()


    handleMap ::  ProgState ()
    handleMap = do
        experssion <- pop
        list <- pop
        case list of
            Literal (List x) -> do 
                let literalList = map (\e -> unWrapStackLiteral ( head $ executeCodeLine (unWrap experssion) ( (M.empty :: AssignmentMap),([Literal e]) ) ))  x
                push $ Literal $ List literalList
                -- concatState t
        -- currentStack <- get
                return ()

        
    handleFold ::  ProgState ()
    handleFold = do
        op <- pop
        acc <- pop
        list <- pop
        -- TODO: error, handle different error cases
        case list of
            Literal (List listLiteral) -> do 
                let res = fold' listLiteral op [acc]
                push $ head res
                return ()


    fold' :: [StackLiteral] -> Ops -> Stack -> Stack
    fold' [] _ stack = stack 
    fold' (x:xs) op stack = 
        let foldStack = executeCodeLine (unWrap op) ( M.empty :: AssignmentMap,stack ++ [Literal x] )
        in fold' xs op foldStack
        

    handleTimes :: ProgState()
    handleTimes = do
        expression <- pop
        time <- popAndEval
        case time of
            Literal num -> do
                let res = times num expression
                concatState res
                return ()



    times :: StackLiteral -> StackElement -> Stack
    times (StackInt num) (Exec expr) =
        let timesWrapper (StackInt 0) _ stack = stack
            timesWrapper (StackInt num) (Exec expr) stack = timesWrapper ((StackInt num) - StackInt 1) (Exec expr) (stack ++ executeCodeLine expr ( (M.empty :: AssignmentMap),([])))
        in
            timesWrapper (StackInt num) (Exec expr) []
    times _ expression = [] -- TODO: error handling
    times (StackInt num) _ = [] -- TODO: error handling

    -- should added to all operators that don't requrire back lookup?
    -- evalValues :: StackElement -> Bool
    -- evalValues token = case token of
    --     Literal _ -> True
    --     Exec _ -> True
    --     Arithmetic _ -> True
    --     otherwise -> False



    -- removeOp :: StackElement -> Bool
    -- removeOp token = case token of
    --     Literal _ -> False
    --     Exec _ -> False
    --     Arithmetic _ -> False
    --     otherwise -> True