module Stack.Stack (
    stackManip
 ) where
    import Control.Monad.State
    import Types

    import Operations.StackOp
    import Operations.Arithmetic
    import Operations.ListOp
    import Stack.StackOperations



    stackManip ::  ProgState
    stackManip = do
        currentStack <- get
        let removeTokens = (filter (\token -> removeOp token) currentStack )
        put removeTokens 
        changeState currentStack
        newStack <- get
        return newStack



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
        otherwise -> return ()
        

    removeOp :: TokenType a -> Bool
    removeOp token = case token of
        Literal _ -> True
        List _ -> True
        otherwise -> False
