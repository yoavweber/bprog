module Operations.Arithmetic where
    import Text.Read (readMaybe)
    import Control.Monad.State
    import qualified Data.Map.Strict as M


    import Stack.StackOperations(pop, popFromEnd,push,getVarMap,popAndEval,stackIsEmpty,push)
    import Types

    -- TODO: try to create a generic function which getting as an input the operation
    handleAritmic :: StackElement -> ProgState ()
    handleAritmic t = case t of
        Arithmetic "+" -> opAdd
        Arithmetic "==" -> opEq
        Arithmetic "*" -> opMult 
        Arithmetic "-" -> opMin
        -- otherwise ->  put [Arithmetics "1"]

    
    opMult :: ProgState ()
    opMult = do
        (ignore,stack) <- get
        case length stack < 2 of 
            True -> do 
                push (Arithmetic "*")
                return ()
            False -> do
                a <- popAndEval
                b <- popAndEval 
                let res =  a * b
                push res
                -- put (ignore, newStack)
                return()


        -- if there is not enough element, push it as execution that would be trigerd on another element
    opMin :: ProgState ()
    opMin = do
        (ignore,stack) <- get
        case length stack < 2 of 
            True -> do 
                push (Arithmetic "-")
                return ()
            False -> do
                a <- popAndEval
                b <- popAndEval 
                let res =  b - a 
                push res
                return()                        
    

    -- if there is not enough element, push it as execution that would be trigerd on another element
    opAdd :: ProgState ()
    opAdd = do
        (ignore,stack) <- get
        case length stack < 2 of 
            True -> do 
                push (Arithmetic "+")
                return ()
            False -> do
                a <- popAndEval
                b <- popAndEval 
                let res =  a + b
                push res
                return()
            


    
    opEq :: ProgState ()
    opEq = do
        a <- popAndEval
        b <- popAndEval
        let res = a == b
        case res of
            False -> push (Literal (StackBool False))
            otherwise -> push (Literal (StackBool True))




                