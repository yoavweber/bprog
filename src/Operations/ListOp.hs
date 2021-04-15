module Operations.ListOp where
    import Control.Monad.State
    import Stack.StackOperations(pop, push)

    import Types


    handleListOp :: String -> State Stack ()
    handleListOp t = case t of 
        "head" -> handleHead
        -- "length" -> handleLength

    handleHead :: State Stack ()
    handleHead = do
        list <- pop
        -- push $ head' list
        return ()

    -- handleLength :: State Stack ()
    -- handleLength = do
    --     list <- pop
    --     -- push $ length' list
    --     return ()



    -- handleappend :: State Stack ()
    -- handleappend = do
    --     list <- pop
    --     list <- pop
    --     pop
    --     push $ head' list
    --     return ()

    head' :: [StackLiteral]  -> StackLiteral 
    head' x = head x 
        
        -- case x of
        -- List l -> StackString (head l)


    -- tail' :: TokenType [a] -> TokenType [a]
    -- tail' x = case x of
    --     List l -> Literal (head $ reverse l)

    
    -- length' :: TokenType [a] -> StackElement
    -- length' x = case x of
    --     List l -> Literal (show $ length l)
            
            


    -- checkLiteral1 :: TokenType a -> TokenType a 
--   head' :: TokenType [a] -> TokenType [a]
--     head' list = case list of
--         List l -> if (head l) == "["
--             then List l
--             else Literal (head l)