{-# LANGUAGE RankNTypes #-}
module Operations.Arithmetic where
    import Text.Read (readMaybe)
    import Control.Monad.State
    import Control.Monad

    import qualified Data.Map.Strict as M

    import Stack.StateOps(pop, popFromEnd,push,getVarMap,popAndEval,stackIsEmpty,push,checkStackLength)
    import Types
    import Text.Read(readMaybe)

    type OpArithmitic = StackElement -> StackElement -> (forall a. (Num a) => a -> a -> a) -> StackElement 
    type OpBool =  StackElement -> StackElement -> (forall a. (Num a) => a -> a -> Bool) -> StackElement 

    -- TODO: try to create a generic function which getting as an input the operation
    handleAritmic :: StackElement -> ProgState ()
    handleAritmic t = case t of
        Arithmetic "+" ->  handleOp  "+" (+)
        Arithmetic "*" ->  handleOp  "*" (*) 
        Arithmetic "-" ->  handleOp  "-" (-)
        Arithmetic "div" -> handleDiv div'  "div"
        Arithmetic "/" -> handleDiv divFrac  "/"
        Arithmetic "==" ->  handleBool "==" (==)
        Arithmetic "<" ->  handleBool "<" (<)
        Arithmetic ">" ->  handleBool ">" (>)
        Arithmetic "&&" -> handleLogic "&&" (&&)
        Arithmetic "||" -> handleLogic "||" (||)
        Arithmetic "not" -> handleNot

    handleOp ::  String -> (forall a. (Num a) => a -> a -> a)  ->  ProgState ()
    handleOp  opString op = do
        preformOp <- checkStackLength (Arithmetic opString) 2
        Control.Monad.when preformOp $ do
            a <- popAndEval
            b <- popAndEval
            let res = opArithmitic a b op 
            push res
            return ()

    handleDiv :: (StackElement -> StackElement -> StackElement ) -> String -> ProgState ()
    handleDiv f opString = do
        preformOp <- checkStackLength (Arithmetic opString) 2
        Control.Monad.when preformOp $ do
            a <- popAndEval
            b <- popAndEval
            let res = f a b 
            push res
            return ()

    handleBool ::  String -> (forall a. (Ord a) => a -> a -> Bool)  ->  ProgState ()
    handleBool  opString op = do
        preformOp <- checkStackLength (Arithmetic opString) 2
        Control.Monad.when preformOp $ do
            a <- popAndEval
            b <- popAndEval
            let res = opBool a b op 
            push res
            return ()

    handleLogic ::  String -> (Bool  -> Bool  -> Bool )  ->  ProgState ()
    handleLogic opString f  = do
        preformOp <- checkStackLength (Arithmetic opString) 2
        Control.Monad.when preformOp $ do
            a <- popAndEval
            b <- popAndEval
            let res = opLogic a b f
            push res
            return ()
  

    handleNot :: ProgState ()
    handleNot = do 
        preformOp <- checkStackLength (Arithmetic "not") 1
        Control.Monad.when preformOp $ do
            a <- popAndEval
            let res = not' a
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
                push $ Literal (List $ map (Literal . StackString) (words str))  
                return ()
            _ -> do
                push maybeString
                push $ Error  "could not use function words on non-string type"


  
   
    opArithmitic :: StackElement -> StackElement -> (forall a. (Num a) => a -> a -> a) -> StackElement 
    opArithmitic (Literal (StackInt a)) (Literal (StackInt b)) f = Literal $ StackInt $ f b a
    opArithmitic (Literal (StackInt a)) (Literal (StackFloat b)) f = Literal $ StackFloat $  f b $ fromIntegral a
    opArithmitic (Literal (StackFloat b)) (Literal (StackInt a)) f = Literal $ StackFloat $  f b $ fromIntegral a
    opArithmitic (Literal (StackFloat a)) (Literal (StackFloat b)) f = Literal $ StackFloat $  f b a
    opArithmitic (Literal (StackFloat a)) _ _ = Error "Error: can only preform arithmic operation on float or int"
    opArithmitic (Literal (StackInt a)) _ _ = Error "Error: can only preform arithmic operation on float or int"
    opArithmitic _ _ _ = Error "Error: can only preform arithmic operation on float or int"


    opBool :: StackElement -> StackElement -> (forall a. (Ord a) => a -> a -> Bool) -> StackElement 
    opBool (Literal (StackInt a)) (Literal (StackInt b)) f = Literal $ StackBool $ f b a
    opBool (Literal (StackInt a)) (Literal (StackFloat b)) f = Literal $ StackBool $ f b $ fromIntegral a
    opBool (Literal (StackFloat a)) (Literal (StackInt b)) f = Literal $ StackBool $ f (fromIntegral b) a 
    opBool (Literal (StackFloat a)) (Literal (StackFloat b)) f = Literal $ StackBool $ f b a
    opBool (Literal (StackBool a)) (Literal (StackBool b)) f = Literal $ StackBool $ f b a
    opBool (Literal (StackString a)) (Literal (StackString b)) f = Literal $ StackBool $ f b a
    opBool  (Literal (List a)) (Literal (List b)) f = Literal $ StackBool $ f b a
    opBool (Literal (StackFloat a)) _ _ = Error "Error: can only preform arithmic operation on float or int"
    opBool (Literal (StackInt a)) _ _ = Error "Error: can only preform arithmic operation on float or int"
    opBool _ _ _ = Error "Error: can only preform arithmic operation on float or int"

    
    opLogic :: StackElement -> StackElement -> (Bool -> Bool -> Bool) -> StackElement 
    opLogic (Literal (StackBool a)) (Literal (StackBool b)) f = Literal (StackBool (f a b)) 
    opLogic _ _ _ = Error "Error : can only perform logical operation on bool" 

    not' :: StackElement -> StackElement
    not' (Literal (StackBool a)) = Literal (StackBool (not a)) 
    not' _ = Error "Error: Can't preform not on not bool type" 



    div' :: StackElement -> StackElement -> StackElement 
    div' (Literal (StackInt 0)) _  = Error "Error: can't devide by zero" 
    div' (Literal (StackFloat 0)) _  = Error "Error: can't devide by zero" 
    div' (Literal (StackInt a)) (Literal (StackInt b)) = Literal $ StackInt $ b `div` a
    div' (Literal (StackInt a)) (Literal (StackFloat b)) = Literal $ StackInt $ round b `div`  a
    div' (Literal (StackFloat a)) (Literal (StackFloat b)) = Literal $ StackInt $ round b `div` round a
    div' (Literal (StackFloat a)) (Literal (StackInt b)) = Literal $ StackInt $ b `div` round a
    div' _ _ = Error "Error: can't devide by this type" 


    divFrac :: StackElement -> StackElement -> StackElement 
    divFrac (Literal (StackFloat 0)) _  = Error "Error: can't devide by zero"
    divFrac (Literal (StackInt 0)) _  = Error "Error: can't devide by zero"
    divFrac (Literal (StackFloat a)) (Literal (StackFloat b)) = Literal $ StackFloat $ b / a
    divFrac (Literal (StackFloat a)) (Literal (StackInt b)) = Literal $ StackFloat $ fromIntegral b / a
    divFrac (Literal (StackInt a)) (Literal (StackFloat b)) = Literal $ StackFloat $ b / fromIntegral a
    divFrac (Literal (StackInt a)) (Literal (StackInt b)) = Literal $ StackFloat $ fromIntegral b / fromIntegral a
    divFrac _ _ = Error "Error: can't devide by this type" 


