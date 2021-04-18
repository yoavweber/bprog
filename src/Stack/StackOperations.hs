module Stack.StackOperations (
    pop,
    push,
    getVarMap,
    peekStack,
    updateVar
 ) where
    import Control.Monad.State

    import Types


   -- TODO: change the name of the file to manage state
   -- TODO: change the naming from xy to more useful
    pop :: ProgState (StackElement)
    pop = state $ \(y,(x:xs)) -> (x,(y,xs))

    push :: StackElement -> ProgState ()
    push a = state $ \(y,xs) -> ((),(y,a:xs))


    
   --  picking the first element from the stack
    peekStack :: ProgState (StackElement)
    peekStack = state $ \(y,x:xs) -> (x,(y,x:xs))

    getVarMap ::  ProgState (AssignmentMap)
    getVarMap = state $ \(y,x) -> (y,(y,x))

   --  Updating the varible map
    updateVar :: AssignmentMap -> ProgState ()
    updateVar newMap =  state $ \(map,stack) -> ( (),(newMap,stack) )