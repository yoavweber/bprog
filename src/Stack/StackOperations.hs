module Stack.StackOperations (
    pop,
    push,
    pick,
    pick2,
    updateVar
 ) where
    import Control.Monad.State

    import Types


   -- TODO: change the name of the file to stack managment
    pop :: State Stack (StackElement)
    pop = state $ \(x:xs) -> (x,xs)

    push :: StackElement -> State Stack ()
    push a = state $ \xs -> ((),a:xs)

   -- getting the last the map from the stack
    pick ::  State Stack (StackElement)
    pick = state $ \(xs) -> ((last xs),xs)

    pick2 ::  State Stack (StackElement)
    pick2 = state $ \(x:xs) -> (x,x:xs)



    updateVar :: StackElement -> State Stack ()
    updateVar a =  state $ \(xs) -> ( (),init(xs) ++ [a] )