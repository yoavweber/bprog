module Operations.Arithmetic where
    import Control.Monad.State
    import qualified Data.Map.Strict as M

    import Stack.StateOps(pop, popFromEnd,push,getVarMap,popAndEval,stackIsEmpty,push)
    import Types
    import Text.Read(readMaybe)

    -- TODO: try to create a generic function which getting as an input the operation
    handleAritmic :: StackElement -> ProgState ()
    handleAritmic t = case t of
        Arithmetic "+" -> opAdd
        Arithmetic "div" -> op div'
        Arithmetic "/" -> op divFrac
        Arithmetic "==" -> opEq
        Arithmetic "*" -> opMult 
        Arithmetic "-" -> opMin
        Arithmetic "<" -> opBool (<)
        Arithmetic ">" -> opBool (>)
        Arithmetic "parseFloat" -> parseFloat
        Arithmetic "parseInteger" -> parseInt
        Arithmetic "words" -> stackWords
        -- otherwise ->  put [Arithmetics "1"]

    op :: (StackElement   -> StackElement  -> StackElement  ) ->  ProgState ()
    op f = do
        (ignore,stack) <- get
        if length stack < 2 then do 
            push (Arithmetic "*")
            return ()
        else do 
            a <- popAndEval
            b <- popAndEval
            let res = f a  b
            push res
            return ()

    parseFloat :: ProgState ()
    parseFloat = do
        maybeFloat <- pop
        case maybeFloat of
            Literal (StackString float) -> do
                case readMaybe float :: Maybe Float of
                    Just n -> do
                        push $ Literal (StackFloat  n)
                        return ()
                    Nothing -> do
                        push $ Error "Could not parse float"
                        return ()
            _ -> do 
                push $ Error "parseFloat can only be used on strings"
                return ()

    parseInt :: ProgState ()
    parseInt = do
        maybeInt <- pop
        case maybeInt of
            Literal (StackString int) -> do
                case (readMaybe int :: Maybe Integer) of
                    Just n -> do
                        push $ Literal (StackInt  n)
                        return ()
                    Nothing -> do
                        push $ Error "Could not parse Int"
                        return ()
            _ -> do 
                push $ Error "parseInt can only be used on strings"
                return ()
                
    stackWords :: ProgState ()
    stackWords = do
        maybeString <- pop
        case maybeString of
            Literal (StackString str) -> do
                push $ Literal (List $ map StackString (words str))  
                return ()
            _ -> do
                push maybeString
                push $ Error  "could not use function words on non-string type"


    opBool :: (StackElement   -> StackElement  -> Bool  ) ->  ProgState ()
    opBool f = do
        (ignore,stack) <- get
        if length stack < 2 then do 
            push (Arithmetic "*")
            return ()
        else do 
            a <- popAndEval
            b <- popAndEval
            let res = f a  b
            push $ Literal (StackBool res)
            return ()
    

    
    -- TODO: create a gloabl stack function that check if stack is empty
    
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
                -- TODO: error, push error instead
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


    div' :: StackElement -> StackElement -> StackElement 
    div' (Literal (StackInt a)) 0 = Literal (StackString "can't devide by zero") -- TODO: error, 
    div' (Literal (StackInt a)) (Literal (StackInt b)) = Literal $ StackInt $ fromInteger $  toInteger b  `div` toInteger  a
    div' _ _ = Literal (StackString "can't devide by this type") -- TODO: error, 


    divFrac :: StackElement -> StackElement -> StackElement 
    divFrac (Literal (StackInt a)) 0 = Literal (StackString "can't devide by zero")
    divFrac (Literal (StackFloat a)) (Literal (StackFloat b)) = Literal $ StackFloat $ b / a