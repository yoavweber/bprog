module Operations.Logical where
    import Control.Monad.State
    import Types

    -- -- - TODO: try to create a generic function which getting as an input the operation
    -- handleAritmic :: StackElement -> State Stack ()
    -- handleLogic t = case t of
    --     Arithmetics "" -> opAdd
    --     otherwise ->  put [Arithmetics "1"]


    -- -- unWrap :: Ops -> a
    -- -- unWrap a = case a of
    -- --     Literals t -> t 
    -- --     -- Exec t -> t

    -- -- refactor to use applicative 
    -- opAdd :: State Stack ()
    -- opAdd = do
    --     a <- pop
    --     b <- pop
    --     let res = a + b
    --     case res of
    --         Literals (StackString "error!") -> do
    --             push (Errors "you matched the wrong type")
    --             return ()
    --         otherwise -> do
    --             push res
    --             return ()
            
    --     return ()
