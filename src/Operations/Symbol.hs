module Operations.Symbol where
    import Control.Monad.State
    import qualified Data.Map.Strict as M

    import Stack.StateOps(pop,popFromEnd, push,getVarMap,updateVar,peekStack,push)
    import Parser
    import Types


    handleVariable :: String -> ProgState ()
    -- handleVariable t = do
    --     return ()
    handleVariable t = case t of
        ":=" -> handleAssignment
        -- "func" -> handleFunction



    assignVariable :: StackLiteral -> ProgState ()
    assignVariable var = do
        push $ Literal var
        assignmentMap <- getVarMap 
        case (M.lookup (Literal var) assignmentMap) of
            Nothing -> do
                -- let t = M.insert var (Variable "undefined element") assignmentMap
                let t = M.insert (Literal var) (Variable "undefined element") assignmentMap
                updateVar t
                return ()
            Just n -> do
                -- poping the variable and inserting the value of it
                -- pop
                -- push (Literal n)
                return ()

        

    handleAssignment :: ProgState ()
    handleAssignment = do
        assignmentMap <- getVarMap
        maybeValue <- pop
        symbol <- pop
        case checkValue maybeValue of 
            Nothing -> do
                push $ Literal (StackString "error: undefined value has been inserted to the stack") -- TODO: , error
                -- push maybeValue
                return ()
            Just value -> do
                -- case value of
                -- TODO: if its a litral rise an erro
                let t = M.insert symbol value assignmentMap
                updateVar  t
                return ()

    
    checkValue :: Ops -> Maybe StackLiteral
    checkValue value = case value of
        Literal val -> Just val
        otherwise -> Nothing 