module Stack.StackOperations (
    pop,
    push,
    pick,
 ) where
    import Control.Monad.State

    import Types

    pop :: State Stack (TokenType String)
    pop = state $ \(x:xs) -> (x,xs)

    push :: TokenType String -> State Stack ()
    push a = state $ \xs -> ((),a:xs)

    pick ::  State Stack (TokenType String)
    pick = state $ \(x:xs) -> (x,x:xs)
