module Stack.Stack
    where
    import Control.Monad.State
    import Control.Monad
    import Types
    import qualified Data.Map.Strict as M


    import Operations.StackOp
    import Operations.Arithmetic
    import Operations.ListOp(handleListOp)
    import Operations.Symbol
    import Operations.StringOp
    import Stack.StateOps
    import Parser

    ------------ Handling cotrol flow and execution ------------------
    -- Executing the state operation on the stack
    executeCode :: String -> (AssignmentMap, Stack) -> (AssignmentMap, Stack)
    executeCode line (varMap,previousStack) =
        let stack = previousStack ++  parseLine line
        in execState stackManip (varMap,stack)

    stackManip ::  ProgState ()
    stackManip = do
        ergonomicManip
        (varMap,currentStack) <- get
        put (varMap,[])
        changeState currentStack
        (varMap,unevelStack) <- get
        if StackIO "read" `elem` unevelStack 
            then do
            return ()
            else do
                let newStack = evalStack unevelStack varMap
                put (varMap,[])
                changeState newStack
                return ()

-- ------------------------------------ ergonomic expressions -----------------------------
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
    handleErgonomic e =
        case e of
            ControlFlow "if" -> do
                preformOp <- checkStackLength (ControlFlow "if") 2
                Control.Monad.when preformOp $ do
                    ergonomicExpr True
                    push $ ControlFlow "if"
                    return ()
            ControlFlow "exec" -> do
                push $ ControlFlow "exec"
                return ()
            ControlFlow controlFlow -> do
                preformOp <- checkStackLength (ControlFlow controlFlow) 1
                Control.Monad.when preformOp $ do
                    ergonomicExpr False 
                    push $ ControlFlow controlFlow
                    return ()
            _ -> do
                push e
                return ()


    -- Removing the argument and replacing it with an expression
    ergonomicExpr :: Bool  -> ProgState()
    ergonomicExpr isIf  =
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
        maybeExpr <- pop
        case maybeExpr of
            Exec _ ->
                return maybeExpr
            _ ->
                return $ Exec [maybeExpr]

    ------------- ------ evaluating stack ------------------------------------
       -- Looping over each element in the stack evaluating it
    changeState :: Stack -> ProgState ()
    changeState [] = return ()
    changeState ((StackIO "print"):xs) = do
        handleIO
        concatState xs
        return () 
    changeState ((StackIO e):xs) = do
        concatState (StackIO e:xs)
        return () 
    changeState (x:xs) = do
        handleTokens x
        changeState xs

    -- sedning each token to the function that handle his type
    handleTokens :: StackElement -> ProgState ()
    handleTokens t = case t of
        Arithmetic t ->  handleAritmic  (Arithmetic t)
        StackOp t -> handleStackOp (StackOp t)
        ListOp t -> handleListOp t
        StackIO e -> push (StackIO e) 
        AssignmentOp t -> handleVariable t
        ControlFlow t -> handleControlFlow t
        Exec t -> push (Exec t)
        StringOp t -> handleStringOp t
        Literal (Variable var) -> assignVariable (Variable var)
        Literal var -> push (Literal var)
        Error error -> push (Error error)
        _ -> return ()

    -- checking if the object is printble, if it is push it to stack and if not send an error
    handleIO :: ProgState ()
    handleIO = do
        preformOp <- checkStackLength (StackIO "print") 1
        Control.Monad.when preformOp $ do
            printElement <- popAndEval 
            varMap <- getVarMap
            push $ print' printElement varMap 
            return ()
    
    print' :: StackElement -> AssignmentMap  -> StackElement 
    print' (Literal (StackString e)) _ = StackIO e
    print' _ _ = Error "can't print a nonstring element"

    ---------------------- evaluate varible -----------------
    -- loop over the stack and search for varibles, if there are evaluate them
    evalStack :: Stack -> AssignmentMap -> Stack
    evalStack [] _ = []
    evalStack (maybeVar:stack) assignmentMap = case maybeVar of
        Literal maybeVar -> eval maybeVar assignmentMap : evalStack stack assignmentMap
        StackIO "read" -> maybeVar:stack
        _  -> maybeVar:evalStack stack assignmentMap


   -- checking if the recived literal is a varible, if it is evaluate it and if not return it 
    eval ::  StackLiteral -> AssignmentMap -> StackElement
    eval element assignmentMap =  case element of
        List list -> Literal $ loopLiteralList list assignmentMap
        Variable e -> getVarValue (Literal $ Variable e) assignmentMap
        _ -> Literal element


    -- chacking if the varible has a value or a function, if it is a function send an error     
    loopLiteralList :: [StackElement] -> AssignmentMap  -> StackLiteral
    loopLiteralList list assignmentMap = List $ map (\e -> case e of { Literal (List nestedList) -> Literal $  loopLiteralList nestedList assignmentMap; Literal (Variable _ ) ->   getVarValue e assignmentMap; _ -> e}) list


    -- getting the value of the varible, expecting a stack literal variable as an argument
    getVarValue :: StackElement  -> AssignmentMap  -> StackElement
    getVarValue (Literal (Variable e)) assignmentMap = case M.lookup (Variable e) assignmentMap of
        Nothing -> Error "Internal error, could not find variable"
        Just var -> var


-- ---------------------- Control flow --------------------------------
    --  same as executeCode function, only exept an actuall stack type and not a string
    executeParsedCode :: Stack -> (AssignmentMap, Stack) -> Stack
    executeParsedCode stack (varMap,previousStack) =
        let newStack =  previousStack ++ stack
        in snd (execState stackManip (varMap, newStack))

      -- adding result to the stack
    updateStack :: Stack -> ProgState ()
    updateStack res = do
        (varMap,stack) <- get
        let evalTimes = executeParsedCode res (varMap,stack)
        put(varMap,evalTimes)

    -- unwrap exec type to get its value
    unWrap :: Ops -> Stack
    unWrap a = case a of
        Exec t -> t
        _ -> [Error "expected execution type"]

    handleControlFlow :: String -> ProgState ()
    handleControlFlow t = case t of
        "exec" -> handleExecution
        "if" -> handleIf
        "loop" -> handleLoop
        "map" -> handleMap
        "foldl" -> handleFold
        "times" -> handleTimes
        "each" -> handleEach

    -- poping the execution line and execting it, and then pushing it to the stack
    handleExecution :: ProgState ()
    handleExecution = do
        preformOp <- checkStackLength (ControlFlow "exec") 1
        if preformOp
            then do
                executionLine <- pop
                let command = unWrap executionLine
                let res = executeParsedCode command (M.empty :: AssignmentMap, [])
                updateStack res
                return ()
            else do
                return ()

    -- checking if the varible exist, if it does get his value or execute 
    --  if its not assign to it a value
    assignVariable :: StackLiteral -> ProgState ()
    assignVariable (Variable var) = do
        assignmentMap <- getVarMap
        case M.lookup (Variable var) assignmentMap of
            Nothing -> do
                let t = M.insert (Variable var) ( Literal $ Variable var) assignmentMap
                updateVar t
                push $ Literal $ Variable var
                return ()
            Just n -> do
                case n of
                    Exec stack -> do
                        (varMap,currentStack) <- get
                        let executedFunc = executeParsedCode (currentStack ++ stack) (varMap, [])
                        put(varMap,executedFunc)
                        return ()
                    Literal s-> do
                        push n
                        return ()


    -- executing if statmen, if there are 3 elements in the stack
    handleIf :: ProgState ()
    handleIf = do
        preformOp <- checkStackLength (ControlFlow "if") 3
        Control.Monad.when preformOp $ do
            falseExec <- pop
            trueExec <- pop
            condition <- pop
            case condition of
                Literal (StackBool True)  -> do
                    updateStack (executeParsedCode (unWrap trueExec) (M.empty :: AssignmentMap, []))
                    return ()
                Literal (StackBool False) -> do
                    updateStack (executeParsedCode (unWrap falseExec) (M.empty :: AssignmentMap, []))
                    return ()
                _ -> do
                    push condition
                    push (Error "if input error, you didn't provide bool condition") 
                    return ()
    
    -- checking if there are 2 elements in the stack, 
    -- if there are the function would pop two elements and pass them the the loop helper
    handleLoop :: ProgState ()
    handleLoop = do
        preformOp <- checkStackLength (ControlFlow "loop") 2
        Control.Monad.when preformOp $ do
                loopBody <- pop
                loopCondition <- pop
                loop' loopBody loopCondition
                return ()
    

    -- executing the loop body until a bool condition is met 
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

    -- map over the elements and spread them into the stack 
    handleEach ::  ProgState ()
    handleEach = do
        preformOp <- checkStackLength (ControlFlow "each") 2
        Control.Monad.when preformOp $ do
            expression <- pop
            list <- pop
            let stackElements = map' list expression 
            case stackElements of
                Right elem-> updateStack elem
                Left elem -> push elem
                        
    -- same as each, only adding the elements wraped in list
    handleMap ::  ProgState ()
    handleMap = do
        preformOp <- checkStackLength (ControlFlow "map") 2
        Control.Monad.when preformOp $ do
            expression <- pop
            list <- pop
            let literalList = map' list expression
            case literalList of
                Right elem-> push $ Literal $ List elem
                Left elem -> push elem
          
    -- if a list and expression is been given, map over the list and return the result
    map' :: StackElement -> StackElement -> Either Ops Stack 
    map' (Literal (List list)) (Exec expression) = Right   ( map (\e ->  head $ executeParsedCode expression ( M.empty :: AssignmentMap,[e] ) ) list)
    map' _ (Exec expression) = Left   (Error "Error: Expected a list")
    map' (Literal (List list)) _ = Left (Error "Error: Expected a function")
    map' _ _ =  Left (Error "Error: please provide a list and a function to each")



    handleFold ::  ProgState ()
    handleFold = do
        preformOp <- checkStackLength (ControlFlow "foldl") 3
        Control.Monad.when preformOp $ do
            op <- pop
            acc <- pop
            list <- pop
            case list of
                Literal (List listLiteral) -> do
                    let res = fold' listLiteral op [acc]
                    updateStack res
                    return ()
                _ -> push $ Error "Error: expected list for foldl"

    -- executing each element on the list with the operation and inserting it into the stack
    fold' :: [StackElement] -> Ops -> Stack -> Stack
    fold' [] _ stack = stack
    fold' (x:xs) op stack =
        let foldStack = executeParsedCode (unWrap op) ( M.empty :: AssignmentMap,stack ++ [x] )
        in fold' xs op foldStack

    
    handleTimes :: ProgState()
    handleTimes = do
        preformOp <- checkStackLength (ControlFlow "times") 2
        Control.Monad.when preformOp $ do
            expression <- pop
            time <- popAndEval
            let res = times time expression
            case res of 
                Right elem -> updateStack elem
                Left elem -> push elem


    times :: StackElement  -> StackElement -> Either Ops Stack
    times (Literal (StackInt num)) (Exec expr) =
        let timesWrapper (StackInt 0) _ stack = stack
            timesWrapper (StackInt num) (Exec expr) stack = timesWrapper (StackInt (num - 1)) (Exec expr) (stack ++ executeParsedCode expr ( M.empty :: AssignmentMap,[]))
        in
            Right (timesWrapper (StackInt num) (Exec expr) [])
    times (Literal (StackInt num)) _ = Left $ Error "Error: expected an exprssion for times" 
    times _ (Exec expr) = Left $ Error "Error: expected an int for times"
    times _ _ = Left $ Error "Error: wrong arguments for times"

  
  