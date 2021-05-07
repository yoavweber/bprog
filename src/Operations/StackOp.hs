module Operations.StackOp where
    import Control.Monad.State
    import Stack.StateOps(pop, push,popFromEnd)

    import Types

    handleStackOp :: StackElement -> ProgState ()
    handleStackOp t = case t of
        StackOp "pop" -> do 
            (_,getStack) <- get
            if length getStack < 2
                then do
                    return ()
                    else do
                        pop
                        return ()

        StackOp "swap" -> do
            first <- pop
            second <- pop
            push first
            push second
            return ()
        StackOp "dup" -> do
            first <- pop
            push first
            push first
            return ()
