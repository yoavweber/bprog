module Stack.StackOperations (
    pop,
    push,
    pick,
 ) where
    import Control.Monad.State

    import Types

    pop :: State Stack (StackElement)
    pop = state $ \(x:xs) -> (x,xs)

    push :: StackElement -> State Stack ()
    push a = state $ \xs -> ((),a:xs)

    pick ::  State Stack (StackElement)
    pick = state $ \(x:xs) -> (x,x:xs)
