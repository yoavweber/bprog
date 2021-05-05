module Operations.ListOp(handleListOp) where
    import Control.Monad.State
    import qualified Data.Map.Strict as M
    import Data.List(genericLength)
    import Control.Monad



    import Stack.StateOps(pop, push,checkStackLength)
    import Parser
    import Types


    handleListOp :: String ->  ProgState ()
    handleListOp t = case t of
        "head" -> handleHead
        "tail" -> handleTailOp
        "length" -> handleLength
        "empty" -> handleEmpty
        "cons" -> handleCons
        "append" -> handleAppend


    handleHead ::  ProgState ()
    handleHead  = do
        preformOp <- checkStackLength (ListOp "head") 1
        Control.Monad.when preformOp $ do
            list <- pop
            push $ head' list
            return ()

    handleTailOp :: ProgState ()
    handleTailOp = do
        preformOp <- checkStackLength (ListOp "tail") 1
        Control.Monad.when preformOp $ do
            list <- pop
            push $  tail' list
              

    handleLength :: ProgState ()
    handleLength = do
        preformOp <- checkStackLength (ListOp "length") 1
        Control.Monad.when preformOp $ do
            list <- pop
            push $ length' list
                

    handleEmpty :: ProgState ()
    handleEmpty = do
        preformOp <- checkStackLength (ListOp "empty") 1
        Control.Monad.when preformOp $ do
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
        preformOp <- checkStackLength (ListOp "cons") 2
        Control.Monad.when preformOp $ do
            list <- pop
            elem <- pop
            case elem of
                Literal x -> do
                    push $ cons' list (Literal x)
                    return  ()
                _  -> do
                    push $ Error "you tried to con wrong element(do better error handling)"
                    return ()

    handleAppend :: ProgState ()
    handleAppend = do
        preformOp <- checkStackLength (ListOp "append") 2
        Control.Monad.when preformOp $ do
            if preformOp
                then do
                    list2 <- pop
                    list1 <- pop
                    push $ append' list1 list2
                    else do
                        return ()



    append' :: StackElement -> StackElement -> StackElement
    append' (Literal (List list1)) (Literal (List list2)) = Literal $ List (list1 ++ list2)
    append' _ _ = Error "Expteced 2 lists to preform append"

    cons' :: StackElement -> StackElement  -> StackElement
    cons' x a = case x of
        Literal (List list) -> Literal $ List(a:list)
        _ -> Error "Expteced a list to preform cons"


    tail' :: StackElement -> StackElement
    tail' x = case x of
        Literal (List []) -> Error "can't do tail on empty list"
        Literal (List list) -> Literal $ List (tail list )
        _ -> Error "Expteced a list to preform tail"

    length' :: StackElement -> StackElement
    length' x = case x of
        Literal (List l) -> Literal $ StackInt (genericLength l)
        Literal (StackString str) -> Literal $ StackInt (genericLength str)
        Exec list -> Literal $ StackInt (genericLength list)
        _ -> Error "can't preform length on this type, expected a string or a list"

    head' :: StackElement -> StackElement
    head' (Literal (List list)) = head list
    head' _ = Error "Expteced a list to preform head"
