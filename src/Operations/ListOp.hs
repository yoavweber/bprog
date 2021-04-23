module Operations.ListOp(handleListOp) where
    import Control.Monad.State
    import qualified Data.Map.Strict as M
    
    import Stack.StateOps(pop, push)
    import Parser
    import Types


    handleListOp :: String -> ProgState ()
    handleListOp t = case t of 
        "head" -> handleHead
        -- "tail" -> handleTail 
        "length" -> handleLength
        -- "map"
        -- "length" -> handleLength

    handleHead :: ProgState ()
    handleHead = do
        list <- pop
        push $ head' list
        return ()

    handleLength :: ProgState ()
    handleLength = do
        list <- pop
        push $ length' list
        return ()



    -- handleappend :: State Stack ()
    -- handleappend = do
    --     list <- pop
    --     list <- pop
    --     pop
    --     push $ head' list
    --     return ()
    -- TODO: check the type of the new element that you are pushing to the stack
    head' :: StackElement  -> StackElement 
    head' x = case x of
       Literal (List l) -> Literal ((head l)) 
        
        -- case x of
        -- List l -> StackString (head l)


    -- tail' :: StackElement  -> StackElement 
    -- tail' x = case x of
    --     List l -> Literal (last l)

    
    length' :: StackElement -> StackElement
    length' x = case x of
        Literal ( List l) -> Literal $ StackInt (length l)





    -- executeCodeLine :: String -> (AssignmentMap, Stack) -> Stack
    -- executeCodeLine line (varMap,previousStack) = 
    --     let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ previousStack
    --     in snd (execState stackManip (varMap, stack))
        

    -- handleMap :: StackElement -> StackElement -> ProgState ()
    -- map' list expr = 
        

            
            


    -- checkLiteral1 :: TokenType a -> TokenType a 
--   head' :: TokenType [a] -> TokenType [a]
--     head' list = case list of
--         List l -> if (head l) == "["
--             then List l
--             else Literal (head l)