module Stack.StackOperations (
    pop,
    push,
    getVarMap,
    peekStack,
    updateVar,
    popAndEval,
    stackIsEmpty
    ,pushToEnd
 ) where
    import Control.Monad.State
    import qualified Data.Map.Strict as M


    import Types


   -- TODO: change the name of the file to manage state
   -- TODO: change the naming from xy to more useful
    pop :: ProgState (StackElement)
    pop = state $ \(y,(x:xs)) -> (x,(y,xs))

    stackIsEmpty :: ProgState(Bool)
    stackIsEmpty = do
       (_,stack) <- get
       if length stack == 0
          then return True
            else return False



    popAndEval :: ProgState (StackElement)
    popAndEval  = do 
       maybeVar <- pop
       case maybeVar of
             Literal (Variable var) -> do
                assignmentMap <- getVarMap
                case M.lookup (Literal (Variable var)) assignmentMap of
                   Nothing -> do
                         return (Literal $ StackString "Error") -- TODO: error, there is no such variable(this is a problem in the program)
                   Just n -> do
                         return $ Literal n
            --  Literal x -> do
            --     return x
             otherwise -> do
                return maybeVar-- TODO: error, 
                

    
    push :: StackElement -> ProgState ()
    push a = state $ \(y,xs) -> ((),(y,a:xs))

    pushToEnd :: StackElement -> ProgState ()
    pushToEnd a = state $ \(y,xs) -> ((),(y,xs ++ [a]))
    
   --  picking the first element from the stack
    peekStack :: ProgState (StackElement)
    peekStack = state $ \(y,x:xs) -> (x,(y,x:xs))

    getVarMap ::  ProgState (AssignmentMap)
    getVarMap = state $ \(y,x) -> (y,(y,x))

   --  Updating the variable map
    updateVar :: AssignmentMap -> ProgState ()
    updateVar newMap =  state $ \(map,stack) -> ( (),(newMap,stack) )