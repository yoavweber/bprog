module Operations.ListOp where
    import Control.Monad.State
    import Stack.StackOperations(pop, push)
    import Parser

    import Types


    handleListOp :: String -> ProgState ()
    handleListOp t = case t of 
        "head" -> handleHead
        -- "length" -> handleLength

    handleHead :: ProgState ()
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
    -- TODO: check the type of the new element that you are pushing to the stack
    -- head' :: StackElement  -> StackElement 
    -- head' x = case x of
    --    Literal (List l) -> Literal (assignLiteral (head l)) 
        
        -- case x of
        -- List l -> StackString (head l)


    -- tail' :: StackElement  -> StackElement 
    -- tail' x = case x of
    --     List l -> Literal (assignLiteral $ head $ reverse l)

    
    -- length' :: TokenType [a] -> StackElement
    -- length' x = case x of
    --     List l -> Literal (show $ length l)
            
            


    -- checkLiteral1 :: TokenType a -> TokenType a 
--   head' :: TokenType [a] -> TokenType [a]
--     head' list = case list of
--         List l -> if (head l) == "["
--             then List l
--             else Literal (head l)