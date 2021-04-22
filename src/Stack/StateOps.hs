module Stack.StateOps (
    pop,
    push,
    getVarMap,
    peekStack,
    updateVar,
    popAndEval,
    stackIsEmpty
    ,pushToEnd
    ,popFromEnd
    ,concatState
 ) where
    import Control.Monad.State
    import qualified Data.Map.Strict as M


    import Types


   -- TODO: change the name of the file to manage state
   -- TODO: change the naming from xy to more useful

   -- gather all of the helper functions to handle the state
    popFromEnd :: ProgState (StackElement)
    popFromEnd = state $ \(y,(x:xs)) -> (x,(y,xs))

    pop :: ProgState (StackElement)
    pop = state $ \(y,(xs)) -> case xs of { [] ->  ((last xs),(y,[])); otherwise -> ((last xs),(y,(init xs)))} 

   --  popFromEnd = state $ \(y,(xs)) -> ((last xs),(y,(init xs)))

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

    
    pushToEnd :: StackElement -> ProgState ()
    pushToEnd a = state $ \(y,xs) -> ((),(y,a:xs))

    push :: StackElement -> ProgState ()
    push a = state $ \(y,xs) -> ((),(y,xs ++ [a]))


    concatState :: [StackElement] -> ProgState ()
    concatState a = state $ \(y,xs) -> ((),(y,xs ++ a))
    
   --  picking the first element from the stack
    peekStack :: ProgState (StackElement)
    peekStack = state $ \(y,x:xs) -> (x,(y,x:xs))

    getVarMap ::  ProgState (AssignmentMap)
    getVarMap = state $ \(y,x) -> (y,(y,x))

   --  Updating the variable map
    updateVar :: AssignmentMap -> ProgState ()
    updateVar newMap =  state $ \(map,stack) -> ( (),(newMap,stack) )