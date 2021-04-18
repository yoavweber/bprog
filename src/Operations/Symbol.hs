module Operations.Symbol where
    import Control.Monad.State
    import qualified Data.Map.Strict as M

    import Stack.StackOperations(pop, push,pick,updateVar,pick2)
    import Parser
    import Types


    handleVarible :: String -> State Stack ()
    -- handleVarible t = do
    --     return ()
    handleVarible t = case t of
        ":=" -> handleAssignment
        otherwise -> pushVarible



    pushVarible :: State Stack ()
    pushVarible = do
        assignmentMap <- pick
        case assignmentMap of
            VaribleStack m -> do
                var <- pick2
                case (M.lookup var m) of
                    Nothing -> do
                        let t = M.insert var (Varible "undefined element") m
                        updateVar (VaribleStack t)
                        return ()
                    Just n -> do
                        -- poping the varible and inserting the value of it
                        -- pop
                        -- push (Literal n)
                        return ()
            otherwise -> return()  -- TODO: error, the first element is not assignment table, 

        

    handleAssignment :: State Stack ()
    handleAssignment = do
        assignmentMap <- pick
        case assignmentMap of
            VaribleStack m -> do
                symbol <- pop
                maybeValue <- pop
                case checkValue maybeValue of 
                    Nothing -> do
                        push $ Literal (StackString "error: undefined value has been inserted to the stack")
                        push maybeValue
                        return ()
                    Just value -> do
                        let t = M.insert symbol value m
                        updateVar (VaribleStack t)
                        return ()
            otherwise -> return() 
        -- return ()

    checkValue :: Ops -> Maybe StackLiteral
    checkValue value = case value of
        Literal val -> Just val
        otherwise -> Nothing 