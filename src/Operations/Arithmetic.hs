module Operations.Arithmetic where
    import Text.Read (readMaybe)
    import Control.Monad.State

    import Stack.StackOperations(pop, push)
    import Types

    -- TODO: try to create a generic function which getting as an input the operation
    handleAritmic :: StackElement -> State Stack ()
    handleAritmic t = case t of
        Arithmetics "+" -> opAdd
        Arithmetics "==" -> opEq 
        -- otherwise ->  put [Arithmetics "1"]


    -- unWrap :: Ops -> a
    -- unWrap a = case a of
    --     Literals t -> t 
    --     -- Exec t -> t

    -- refactor to use applicative 
    opAdd :: State Stack ()
    opAdd = do
        a <- pop
        b <- pop
        let res = a + b
        case res of
            Literals (StackString "error!") -> do
                push (Errors "you matched the wrong type")
                return ()
            otherwise -> do
                push res
                return ()
            
        return ()


    
    opEq :: State Stack ()
    opEq = do
        a <- pop
        b <- pop
        let res = a == b
        case res of
            False -> push (Literals (StackBool False))
            otherwise -> push (Literals (StackBool True))
        return ()

