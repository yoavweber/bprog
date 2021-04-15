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


    handleTokens :: TokenType String -> State Stack ()
    handleTokens t = case t of
        Arithmetic t ->  handleAritmic (Arithmetic t)
        StackOp t -> handleStackOp (StackOp t)
        ListOp t -> handleListOp t
        ControlFlow t -> handleControlFlow t
        otherwise -> return ()
        

    removeOp :: TokenType a -> Bool
    removeOp token = case token of
        Literal _ -> True
        List _ -> True
        Exec _ -> True
        otherwise -> False
    


    handleControlFlow :: String -> State Stack ()
    handleControlFlow t = case t of
        "exec" -> handleExecution 
    

    executeCodeLine :: String -> Stack -> TokenType String
    executeCodeLine line previousStack = 
        let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ previousStack
        in (execState stackManip stack) !! 0
        

    handleExecution :: State Stack ()
    handleExecution = do
        executionLine <- pop
        let command = unWrap executionLine
        let res = executeCodeLine command []
        push (res)
        return ()
