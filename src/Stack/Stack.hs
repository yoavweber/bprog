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
        (varMap,unevelStack) <- get
        let newStack = evalStack unevelStack varMap
        put (varMap,newStack)

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
        _ -> return ()


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



    evalStack :: Stack -> AssignmentMap -> Stack 
    evalStack [] _ = []
    evalStack (maybeVar:stack) assignmentMap = case maybeVar of
        -- Literal $ (Variable var) -> 
        Literal maybeVar -> eval maybeVar assignmentMap : evalStack stack assignmentMap
        -- StackIO -> 
        _  -> evalStack stack assignmentMap

    eval ::  StackLiteral -> AssignmentMap -> StackElement 
    eval element assignmentMap =  case element of
        List list -> Literal $ loopLiteralList list  assignmentMap
        Variable e -> getVarValue (Variable e) assignmentMap
        _ -> Literal element


    -- chacking if the varible has a value or a function, if it is a function send an error     
    loopLiteralList :: [StackLiteral] -> AssignmentMap  -> StackLiteral
    loopLiteralList list assignmentMap = List $ map (\e -> case e of { List nestedList -> loopLiteralList nestedList assignmentMap; Variable _ -> handleValueInList $ getVarValue e assignmentMap; _ -> e}) list
   
    getVarValue :: StackLiteral -> AssignmentMap  -> StackElement   
    getVarValue e assignmentMap = case M.lookup e assignmentMap of
        Nothing -> Error "Internal error, could not find variable"
        Just var -> var


    handleValueInList  :: StackElement  -> StackLiteral 
    handleValueInList e = case e of
        Exec _ -> StackString "Can't write a function to a list" -- TODO: if this token there, delete the list
        Literal var -> var 
        _ ->  StackString "Where does it failing?"

-- ---------------------- Control flow --------------------------------
    executeParsedCode :: Stack -> (AssignmentMap, Stack) -> Stack
    executeParsedCode stack (varMap,previousStack) =
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

    handleControlFlow :: String -> ProgState ()
    handleControlFlow t = case t of
        "exec" -> handleExecution
        "if" -> handleIf
        "loop" -> handleLoop
        "map" -> handleMap
        "foldl" -> handleFold
        "times" -> handleTimes
        "each" -> handleEach



    handleExecution :: ProgState ()
    handleExecution = do
        executionLine <- pop
        let command = unWrap executionLine
        let res = executeParsedCode command (M.empty :: AssignmentMap, [])
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
                        let executedFunc = executeParsedCode (currentStack ++ stack) (varMap, [])
                        put(varMap,executedFunc)
                        return ()
                    Literal s-> do
                        push n
                        return ()


    handleIf :: ProgState ()
    handleIf = do
        (_,currentStack) <- get
        if length currentStack >= 3 
            then (do
            falseExec <- pop
            trueExec <- pop
            condition <- pop
        -- TODO: error, if there are no two execution provide an error
            case condition of
                Literal (StackBool True)  -> do
                    concatState (executeParsedCode (unWrap trueExec) (M.empty :: AssignmentMap, []))
                    stackManip
                    return ()
                Literal (StackBool False) -> do
                    concatState (executeParsedCode (unWrap falseExec) ((M.empty :: AssignmentMap), []))
                    stackManip
                    return ()
                _ -> do
                    push condition
                    push (Literal (StackString "if input error, you didn't provided bool condition")) -- handle error
                    return ()) else do

            -- put ((M.empty :: AssignmentMap),[Literal (StackString $ "if input error, not enough arguments: " ++ (show $ length currentStack) )]) -- handle error with proper if statment
                        push (ControlFlow "if")
                        return ()

    handleLoop :: ProgState ()
    handleLoop = do
        preformOp <- checkStackLength (ControlFlow "loop") 2 
        if preformOp
            then do
                loopBody <- pop
                loopCondition <- pop
                loop' loopBody loopCondition 
            else do
                return ()
    
    
    loop' :: StackElement -> StackElement -> ProgState ()
    loop' loopBody loopCondition = do
        (varMap,currentStack) <- get 
        let loop = executeParsedCode (unWrap loopCondition) (varMap,currentStack)
        if last loop == Literal (StackBool False)
            then do 
                let res = executeParsedCode (unWrap loopBody) (varMap,currentStack)
                put(varMap, res )
                loop' loopBody loopCondition
                else do

                    return ()
        
    handleEach ::  ProgState ()
    handleEach = do
        experssion <- pop
        list <- pop
        case list of
            Literal (List x) -> do
                let stackElements = map (\e ->  head $ executeParsedCode (unWrap experssion) ( M.empty :: AssignmentMap,[Literal e] ) )  x
                concatState stackElements
        -- currentStack <- get
                return ()


    handleMap ::  ProgState ()
    handleMap = do
        experssion <- pop
        list <- pop
        case list of
            Literal (List x) -> do
                let literalList = map (\e -> unWrapStackLiteral ( head $ executeParsedCode (unWrap experssion) ( M.empty :: AssignmentMap,[Literal e] ) ))  x
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
        let foldStack = executeParsedCode (unWrap op) ( M.empty :: AssignmentMap,stack ++ [Literal x] )
        in fold' xs op foldStack


    handleTimes :: ProgState()
    handleTimes = do
        expression <- pop
        time <- popAndEval
        case time of
            Literal num -> do
                let res = times num expression
                updateStack res
                return ()



    times :: StackLiteral -> StackElement -> Stack
    times (StackInt num) (Exec expr) =
        let timesWrapper (StackInt 0) _ stack = stack
            timesWrapper (StackInt num) (Exec expr) stack = timesWrapper (StackInt num - StackInt 1) (Exec expr) (stack ++ executeParsedCode expr ( M.empty :: AssignmentMap,[]))
        in
            timesWrapper (StackInt num) (Exec expr) []
    times _ expression = [] -- TODO: error handling
    times (StackInt num) _ = [] -- TODO: error handling

    updateStack :: Stack -> ProgState ()
    updateStack res = do
        (varMap,stack) <- get
        let evalTimes = executeParsedCode res (varMap,stack)
        put(varMap,evalTimes)
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