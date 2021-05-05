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
    ,checkStackLength
    , slice
 ) where
    import Control.Monad.State
    import qualified Data.Map.Strict as M


    import Types


   -- ----------------- Stack utils operation ----------------
    slice :: [a] -> Int -> Int -> [a]
    slice [] _ _ = []
    slice _ _ 0 = []
    slice (x:xs) 1 n = x:slice xs 1 (n-1)
    slice (_:xs) m n = slice xs (m-1) (n-1)
   -- gather all of the helper functions to handle the state
    popFromEnd :: ProgState StackElement
    popFromEnd = state $ \(y,x:xs) -> (x,(y,xs))

    pop :: ProgState StackElement
    pop = state $ \(y,xs) -> case xs of { [] ->  (Error "Can't pop empty list",(y,[])); _ -> (last xs,(y,init xs))}


    stackIsEmpty :: ProgState Bool
    stackIsEmpty = do
       (_,stack) <- get
       if null stack
          then return True
            else return False


    popAndEval :: ProgState StackElement
    popAndEval  = do
       maybeVar <- pop
       case maybeVar of
             Literal (Variable var) -> do
                assignmentMap <- getVarMap
                case M.lookup ( Variable var) assignmentMap of
                   Nothing ->
                         return (Literal $ StackString "Error") -- TODO: error, there is no such variable(this is a problem in the program)
                   Just n ->
                         return n
             _ ->
                return maybeVar


    pushToEnd :: StackElement -> ProgState ()
    pushToEnd a = state $ \(y,xs) -> ((),(y,a:xs))

   -- pushing element to the stack
    push :: StackElement -> ProgState ()
    push a = state $ \(y,xs) -> ((),(y,xs ++ [a]))

   -- concatenating stack with list 
    concatState :: Stack  -> ProgState ()
    concatState a = state $ \(y,xs) -> ((),(y,xs ++ a))

   --  picking the first element from the stack
    peekStack :: ProgState StackElement
    peekStack = state $ \(y,xs) -> case xs of { [] ->  (Error "Can't peek empty stack",(y,[])); _ -> (last xs,(y, xs))}
   
    -- returning a list with the first few elements 
    peekFewElements :: Int -> ProgState Stack
    peekFewElements num = state $ \(y,xs) -> case xs of { [] ->  ([Error "Can't peek empty stack"],(y,[])); _ -> (slice xs ((length xs -  num) + 1) (length xs),(y, xs))}

   -- Getting the varible map
    getVarMap ::  ProgState AssignmentMap
    getVarMap = state $ \(y,x) -> (y,(y,x))

   --  Updating the variable map
    updateVar :: AssignmentMap -> ProgState ()
    updateVar newMap =  state $ \(map,stack) -> ( (),(newMap,stack) )

   --  checking if the stack have enough elements to preform the operations, and if those operation are not IO
   --  if it don't then push the operation to the stack
    checkStackLength :: StackElement -> Int -> ProgState Bool
    checkStackLength el num = do
       (_,stack) <- get
       res <- checkIO num
       if (length stack < num) || res
          then do
                push el
                return False
                else
                   return True


   -- any of the peekd element is an IO, return false
    checkIO :: Int -> ProgState Bool
    checkIO num = do
       maybeIO <- peekFewElements num
       let lis = filter (\e -> case e of { StackIO _ -> True; _ -> False}) maybeIO
       return (not (null lis))