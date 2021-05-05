module Operations.Symbol where
    import Control.Monad.State
    import Control.Monad
    import qualified Data.Map.Strict as M

    import Stack.StateOps(pop, push,getVarMap,updateVar,peekStack,checkStackLength)
    import Parser
    import Types


    handleVariable :: String -> ProgState ()
    handleVariable t = case t of
        ":=" -> handleAssignment
        "fun" -> assignFunction


    -- assigning value to varible
    handleAssignment :: ProgState ()
    handleAssignment = do
        preformOp <- checkStackLength (AssignmentOp ":=") 2
        Control.Monad.when preformOp $ do
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
                            push $ Error "Error: can't assign varible twice"
                _ -> do 
                    push $ Error "Error: undefined value has been inserted to the stack" -- TODO: , error
                    return ()


    -- assigning value to function
    assignFunction :: ProgState ()
    assignFunction = do 
        preformOp <- checkStackLength (AssignmentOp "fun") 2
        Control.Monad.when preformOp $ do
            assignmentMap <- getVarMap
            maybeExec <- pop
            symbol <- pop
            case symbol of 
                Literal (Variable a) -> case maybeExec of
                    Exec exec -> do
                        let t = M.insert (Variable a) (Exec exec) assignmentMap
                        updateVar t
                        return ()
                _ -> do
                    push $ Literal (StackString "error: undefined value has been inserted to the stack") -- TODO: , error
                    return ()
