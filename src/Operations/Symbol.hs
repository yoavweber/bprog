module Operations.Symbol where
    import Control.Monad.State
    import qualified Data.Map.Strict as M

    import Stack.StackOperations(pop, push,getVarMap,updateVar,peekStack)
    import Parser
    import Types


    handleVarible :: String -> ProgState ()
    -- handleVarible t = do
    --     return ()
    handleVarible t = case t of
        ":=" -> handleAssignment
        otherwise -> pushVarible



    pushVarible :: ProgState ()
    pushVarible = do
        assignmentMap <- getVarMap 
        var <- peekStack
        case (M.lookup var assignmentMap) of
            Nothing -> do
                let t = M.insert var (Varible "undefined element") assignmentMap
                updateVar t
                return ()
            Just n -> do
                -- poping the varible and inserting the value of it
                -- pop
                -- push (Literal n)
                return ()

        

    handleAssignment :: ProgState ()
    handleAssignment = do
        assignmentMap <- getVarMap
        symbol <- pop
        maybeValue <- pop
        case checkValue maybeValue of 
            Nothing -> do
                push $ Literal (StackString "error: undefined value has been inserted to the stack")
                push maybeValue
                return ()
            Just value -> do
                let t = M.insert symbol value assignmentMap
                updateVar  t
                return ()

    checkValue :: Ops -> Maybe StackLiteral
    checkValue value = case value of
        Literal val -> Just val
        otherwise -> Nothing 