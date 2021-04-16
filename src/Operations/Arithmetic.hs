module Operations.Arithmetic where
    import Text.Read (readMaybe)
    import Control.Monad.State

    import Stack.StackOperations(pop, push)
    import Types

    -- TODO: try to create a generic function which getting as an input the operation
    handleAritmic :: StackElement -> State Stack ()
    handleAritmic t = case t of
        Arithmetics "+" -> opAdd
        otherwise ->  put [Arithmetics "1"]


    -- unWrap :: Ops -> a
    -- unWrap a = case a of
    --     Literals t -> t 
    --     -- Exec t -> t

    -- refactor to use applicative 
    opAdd :: State Stack ()
    opAdd = do
        a <- pop
        b <- pop
        return ()
        -- poping the plus sign
        -- pop
        -- let first = unWrap a
        -- let second = unWrap b
        -- case (readMaybe first :: Maybe Int) of
        --     Nothing -> return () -- this should be an error
        --     Just num1 -> case (readMaybe second :: Maybe Int) of
        --         Nothing -> return ()-- This should be an error
        --         Just num2 -> do 
        --             let res = num1 + num2
        --             -- let res =  ((+) <$> num1 <*> num2) 
        --             push (Literal $ show res)
        --             return()

        -- pop
        -- let res =  ((++) <$> a <*> b) 
        -- push res
        -- return()
    -- checkAritmic :: a -> a -> Maybe a
    -- checkAritmic a b = do
