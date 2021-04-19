module Operations.Arithmetic where
    import Text.Read (readMaybe)
    import Control.Monad.State
    import qualified Data.Map.Strict as M


    import Stack.StackOperations(pop, push,getVarMap,popAndEval,stackIsEmpty)
    import Types

    -- TODO: try to create a generic function which getting as an input the operation
    handleAritmic :: StackElement -> ProgState ()
    handleAritmic t = case t of
        Arithmetic "+" -> opAdd
        Arithmetic "==" -> opEq
        Arithmetic "*" -> opMult 
        -- otherwise ->  put [Arithmetics "1"]

    
    
    opMult :: ProgState ()
    opMult = do
        (_,stack) <- get
        case length stack < 2 of 
            True -> do 
                push (Arithmetic "*")
                return ()
            False -> do
                a <- popAndEval
                b <- popAndEval 
                let res =  a * b
                -- res <- ress
                case res of
                    -- StackString "error!" -> do
                    --     push (Error "you matched the wrong type") -- handle error better
                    --     return ()
                    otherwise -> do
                        push (res)
                        return ()
            

    -- if there is not enough element, push it as execution that would be trigerd on another element
    opAdd :: ProgState ()
    opAdd = do
        (_,stack) <- get
        case length stack < 2 of 
            True -> do 
                push (Arithmetic "+")
                return ()
            False -> do
                a <- popAndEval
                b <- popAndEval 
                let res =  a + b
                -- res <- ress
                case res of
                    -- StackString "error!" -> do
                    --     push (Error "you matched the wrong type") -- handle error better
                    --     return ()
                    otherwise -> do
                        push (res)
                        return ()
            


    
    opEq :: ProgState ()
    opEq = do
        a <- popAndEval
        b <- popAndEval
        let res = a == b
        case res of
            False -> push (Literal (StackBool False))
            otherwise -> push (Literal (StackBool True))




    evalVar :: StackElement ->  ProgState (StackLiteral)
    evalVar maybeVar = case maybeVar of
        Literal (Variable var) -> do
            assignmentMap <- getVarMap
            case M.lookup (Literal (Variable var)) assignmentMap of
                Nothing -> do
                    return (StackString "Error") -- TODO: error, there is no such variable(this is a problem in the program)
                Just n -> do
                    return n
        Literal x -> do
            return x
        otherwise -> do
            return (StackString "Error, operation not supported") -- TODO: error, 
                


    -- popAndEval :: ProgState (StackLiteral)
    -- popAndEval  = do 
    --     maybeVar <- pop
    --     case maybeVar of
    --         Literal (Variable var) -> do
    --             assignmentMap <- getVarMap
    --             case M.lookup (Literal (Variable var)) assignmentMap of
    --                 Nothing -> do
    --                     return (StackString "Error") -- TODO: error, there is no such variable(this is a problem in the program)
    --                 Just n -> do
    --                     return n
    --         Literal x -> do
    --             return x
    --         otherwise -> do
    --             return (StackString "Error, operation not supported") -- TODO: error, 
                