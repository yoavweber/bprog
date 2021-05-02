module Operations.Symbol where
    import Control.Monad.State
    import qualified Data.Map.Strict as M

    import Stack.StateOps(pop,popFromEnd, push,getVarMap,updateVar,peekStack,push)
    import Parser
    import Types


    handleVariable :: String -> ProgState ()
    handleVariable t = case t of
        ":=" -> handleAssignment
        "fun" -> assignFunction



        

    handleAssignment :: ProgState ()
    handleAssignment = do
        assignmentMap <- getVarMap
        maybeValue <- pop
        symbol <- pop
        case maybeValue of
            Literal val -> do
                case symbol of
                    Literal (Variable var) -> do
                        let t = M.insert (Variable var) (Literal val) assignmentMap
                        updateVar  t
                        return ()
            _ -> do 
                push $ Literal (StackString "error: undefined value has been inserted to the stack") -- TODO: , error
                return ()

        -- case checkValue maybeValue of 
        --     Nothing -> do
        --         push $ Literal (StackString "error: undefined value has been inserted to the stack") -- TODO: , error
        --         -- push maybeValue
        --         return ()
        --     Just value -> do
        --         -- case value of
        --         -- TODO: if its a litral rise an erro
        --         let t = M.insert symbol value assignmentMap
        --         updateVar  t
        --         return ()


    assignFunction :: ProgState ()
    assignFunction = do 
        -- TODO: error, check if there enogh elemets in the stack
        assignmentMap <- getVarMap
        maybeExec <- pop
        symbol <- pop
        case symbol of 
            Literal (Variable a) -> case maybeExec of
                Exec exec -> do
                -- case value of
                -- TODO: if its a litral rise an erro
                    let t = M.insert (Variable a) (Exec exec) assignmentMap
                    updateVar t
                    return ()
            _ -> do
                push $ Literal (StackString "error: undefined value has been inserted to the stack") -- TODO: , error
                return ()


    checkValue :: Ops -> Maybe StackLiteral
    checkValue value = case value of
        Literal val -> Just val
        otherwise -> Nothing 