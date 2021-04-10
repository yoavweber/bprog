module Stack where
    import Control.Monad.State

    import Types


    pop :: State Stack (TokenType String)
    pop = state $ \(x:xs) -> (x,xs)

    push :: TokenType String -> State Stack ()
    push a = state $ \xs -> ((),a:xs)

    pick ::  State Stack (TokenType String)
    pick = state $ \(x:xs) -> (x,x:xs)


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
        otherwise -> return ()
        


    -- create a folder for all aritmic operations and move this function
    handleAritmic :: TokenType String -> State Stack ()
    handleAritmic t = case t of
        Arithmetic "+" -> return ()
        otherwise ->  put [Arithmetic "1"]