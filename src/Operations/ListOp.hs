module Operations.ListOp(handleListOp) where
    import Control.Monad.State
    import qualified Data.Map.Strict as M
    import Data.List(genericLength)

    
    import Stack.StateOps(pop, push)
    import Parser
    import Types


    handleListOp :: String ->  ProgState ()
    handleListOp t = case t of 
        "head" -> handleSingleOp head
        "tail" -> handleTailOp
        "length" -> handleLength
        "empty" -> handleEmpty
        "cons" -> handleCons
        "append" -> handleAppend
    

  

    handleSingleOp :: ([StackLiteral ] -> StackLiteral) -> ProgState ()
    handleSingleOp op = do
        list <- pop
        push $ singleOp list op
        return ()

    handleTailOp :: ProgState ()
    handleTailOp = do
        preformOp <- checkStackLength (ListOp "tail") 1
        if preformOp 
            then do 
                list <- pop
                push $  tail' list 
                else do
                    return ()



    handleLength :: ProgState ()
    handleLength = do
        preformOp <- checkStackLength (ListOp "length") 1
        if preformOp 
            then do  
                list <- pop
                push $ length' list
                else do
                    return ()
    
    handleEmpty :: ProgState ()
    handleEmpty = do
        list <- pop 
        if length' list == Literal (StackInt 0)
            then do
                push $ Literal(StackBool True)
                return ()
                else do
                    push $ Literal(StackBool False)
                    return ()

    handleCons :: ProgState ()
    handleCons = do
        (_,stack) <- get
        if length stack < 2
            then do
                push $ ListOp "cons"
                return ()
                else do
                    list <- pop
                    elem <- pop
                    case elem of
                        Literal x -> do
                            push $ cons' list x
                            return  ()
                        _  -> do 
                            push $ Literal $  StackString "you tried to con wrong element(do better error handling)"
                            return ()

    handleAppend :: ProgState ()
    handleAppend = do
        preformOp <- checkStackLength (ListOp "append") 2
        if preformOp
            then do   
                list2 <- pop
                list1 <- pop
                push $ append' list1 list2
                else do
                    return ()

    checkStackLength :: StackElement -> Int -> ProgState Bool
    checkStackLength el num = do
        (_,stack) <- get
        if length stack < num
            then do
                push el
                return False
                else do
                    return True




    append' :: StackElement -> StackElement -> StackElement 
    append' (Literal (List list1)) (Literal (List list2)) = Literal $ List (list1 ++ list2) 
    append' _ _ = Error "Expteced 2 lists to preform append" 

    cons' :: StackElement -> StackLiteral -> StackElement  
    cons' x a = case x of
        Literal (List list) -> Literal $ List(a:list) 
        _ -> Error "Expteced a list to preform cons"
     

    tail' :: StackElement -> StackElement  
    tail' x = case x of
        Literal (List []) -> Error "can't do tail on empty list"  
        Literal (List list) -> Literal $ List (tail list )
        _ -> Error "Expteced a list to preform tail"



    -- TODO: check the type of the new element that you are pushing to the stack
    singleOp :: StackElement -> ( [StackLiteral] -> StackLiteral  ) -> StackElement 
    singleOp x f  = case x of
       Literal (List l) -> Literal (f l) 
        
    
    length' :: StackElement -> StackElement
    length' x = case x of
        Literal (List l) -> Literal $ StackInt (genericLength l)
        Literal (StackString str) -> Literal $ StackInt (genericLength str)
        _ -> Error "can't preform length on this type, expected a string or a list"

