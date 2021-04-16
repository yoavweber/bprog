module Stack.Stack (
    stackManip,
    executeCode
 ) where
    import Control.Monad.State
    import Types

    import Operations.StackOp
    import Operations.Arithmetic
    import Operations.ListOp
    import Stack.StackOperations
    import Parser



    executeCode :: String -> Stack -> Stack
    executeCode line previousStack = 
        let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ previousStack
        in execState stackManip stack

    stackManip ::  State Stack ()
    stackManip = do
        currentStack <- get
        let removeTokens = (filter (\token -> removeOp token) currentStack )
        put removeTokens 
        changeState currentStack
        newStack <- get
        return ()



    changeState :: Stack -> State Stack ()
    changeState [] = return () 
    changeState (x:xs) = do
        handleTokens x
        changeState xs


    handleTokens :: StackElement -> State Stack ()
    handleTokens t = case t of
        Arithmetics t ->  handleAritmic  (Arithmetics t)
        -- StackOp t -> handleStackOp (StackOp t)
        ListOp t -> handleListOp t
        ControlFlow t -> handleControlFlow t
        otherwise -> return ()
        

    removeOp :: StackElement -> Bool
    removeOp token = case token of
        -- Literal _ -> True
        Literals _ -> True
        Exec _ -> True
        otherwise -> False
    


    handleControlFlow :: String -> State Stack ()
    handleControlFlow t = case t of
        "exec" -> handleExecution 
        "if" -> handleIf
    

    executeCodeLine :: String -> Stack -> StackElement
    executeCodeLine line previousStack = 
        let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ previousStack
        in (execState stackManip stack) !! 0
    


    unWrap :: Ops -> String
    unWrap a = case a of
        -- Literals t -> t 
        Exec t -> t

    handleExecution :: State Stack ()
    handleExecution = do
        executionLine <- pop
        let command = unWrap executionLine
        let res = executeCodeLine command []
        push (res)
        return ()


    handleIf :: State Stack ()
    handleIf = do
        currentStack <- get
        case (length currentStack) >= 3 of
            True -> do
                condition <- pop
                trueExec <- pop
                falseExec <- pop
            -- if there are no two execution provide an error
                case condition of
                    Literals (StackBool True)  -> do
                        push (executeCodeLine (unWrap trueExec) [])
                        return ()
                    Literals (StackBool False) -> do
                        push (executeCodeLine (unWrap falseExec) [])
                        return ()
                    otherwise -> do
                        push (Literals (StackString "if input error, you didn't provided bool condition")) -- handle error
                        return ()
            False -> do 
                put ([Literals (StackString "if input error")]) -- handle error with proper if statment
                return ()
                