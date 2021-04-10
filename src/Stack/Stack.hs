module Stack.Stack (
    stackManip
 ) where
    import Control.Monad.State
    import Types
    import Operations.StackOp

    import Stack.StackOperations



    stackManip ::  ProgState
    stackManip = do
        stackNow <- get
        changeState stackNow
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
        otherwise -> return ()
        


    -- create a folder for all aritmic operations and move this function
    handleAritmic :: TokenType String -> State Stack ()
    handleAritmic t = case t of
        Arithmetic "+" -> return ()
        otherwise ->  put [Arithmetic "1"]