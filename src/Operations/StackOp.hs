module Operations.StackOp where
    import Control.Monad.State
    import Stack.StackOperations(pop, push)

    import Types


    handleStackOp :: TokenType String -> State Stack ()
    handleStackOp t = case t of
        StackOp "pop" -> do 
            pop
            pop
            return ()
        StackOp "swap" -> do
            pop
            first <- pop
            second <- pop
            push first
            push second
            return ()
        StackOp "dup" -> do
            pop
            first <- pop
            push first
            push first
            return ()
