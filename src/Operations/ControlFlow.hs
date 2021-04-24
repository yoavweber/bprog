module Operations.ControlFlow where
    -- import Control.Monad.State
    -- import qualified Data.Map.Strict as M

    -- import Types
    -- import Stack.Stack
    -- import Stack.Exec
    -- import Stack.StateOps(pop, popFromEnd,push,getVarMap,popAndEval,stackIsEmpty,push)


    -- handleControlFlow :: String -> ProgState ()
    -- handleControlFlow t = case t of
    --     "exec" -> handleExecution 
    --     "if" -> handleIf
    

    -- handleExecution :: ProgState ()
    -- handleExecution = do
    --     executionLine <- pop
    --     let command = unWrap executionLine
    --     let res = executeCodeLine command ((M.empty :: AssignmentMap), [])
    --     concatState (res)
    --     return ()


    -- handleIf :: ProgState ()
    -- handleIf = do
    --     (_,currentStack) <- get
    --     case (length currentStack) >= 3 of
    --         True -> do
    --             falseExec <- pop
    --             trueExec <- pop
    --             condition <- pop
    --         -- TODO: error, if there are no two execution provide an error
    --             case condition of
    --                 Literal (StackBool True)  -> do
    --                     concatState (executeCodeLine (unWrap trueExec) ((M.empty :: AssignmentMap), []))
    --                     stackManip
    --                     return ()
    --                 Literal (StackBool False) -> do
    --                     concatState (executeCodeLine (unWrap falseExec) ((M.empty :: AssignmentMap), []))
    --                     stackManip
    --                     return ()
    --                 otherwise -> do
    --                     push (Literal (StackString "if input error, you didn't provided bool condition")) -- handle error
    --                     return ()
    --         False -> do 
    --             -- put ((M.empty :: AssignmentMap),[Literal (StackString $ "if input error, not enough arguments: " ++ (show $ length currentStack) )]) -- handle error with proper if statment
    --             push (ControlFlow "if")
    --             return ()