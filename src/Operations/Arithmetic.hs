module Operations.Arithmetic where
    import Text.Read (readMaybe)
    import Control.Monad.State
    import qualified Data.Map.Strict as M


    import Stack.StackOperations(pop, push,getVarMap)
    import Types

    -- TODO: try to create a generic function which getting as an input the operation
    handleAritmic :: StackElement -> ProgState ()
    handleAritmic t = case t of
        Arithmetic "+" -> opAdd
        Arithmetic "==" -> opEq 
        -- otherwise ->  put [Arithmetics "1"]


    -- unWrap :: Ops -> a
    -- unWrap a = case a of
    --     Literal t -> t 
    --     -- Exec t -> t

    -- if there is not enough element, push it as execution that would be trigerd on another element
    opAdd :: ProgState ()
    opAdd = do
        a <- pop
        b <- pop
        a1 <- evalVar a
        b1 <- evalVar b
        let res =  a1 + b1
        -- res <- ress
        case res of
            -- StackString "error!" -> do
            --     push (Error "you matched the wrong type") -- handle error better
            --     return ()
            otherwise -> do
                push (Literal res)
                return ()
            


    
    opEq :: ProgState ()
    opEq = do
        a <- pop
        b <- pop
        let res = a == b
        case res of
            False -> push (Literal (StackBool False))
            otherwise -> push (Literal (StackBool True))
        -- return ()



    evalVar :: StackElement ->  ProgState (StackLiteral)
    evalVar maybeVar = case maybeVar of
        Literal (Varible var) -> do
            assignmentMap <- getVarMap
            case M.lookup (Literal (Varible var))  assignmentMap of
                Nothing -> do
                    return (StackString "Error") -- TODO: error, there is no such varible(this is a problem in the program)
                Just n -> do
                    return n
        Literal x -> do
            return x
        otherwise -> do
            return (StackString "Error, operation not supported") -- TODO: error, 
                


            
    -- this should change by the way I am structring state
    -- getVaribles:: StackElement -> State Stack (Maybe VaribleStack)
    -- getVaribles = do
    --     assignmentMap <- pick
    --     case assignmentMap
