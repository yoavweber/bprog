module Operations.StringOp where
    import Control.Monad.State
    import Control.Monad

    import Stack.StateOps(pop, popFromEnd,push,getVarMap,popAndEval,stackIsEmpty,push,checkStackLength)
    import Types
    import Text.Read(readMaybe)


    handleStringOp :: String  -> ProgState ()
    handleStringOp t = case t of
        "parseFloat" -> parseFloat
        "parseInteger" -> parseInt
        "words" -> stackWords
  

    parseFloat :: ProgState ()
    parseFloat = do
        preformOp <- checkStackLength (StringOp "parseInteger") 1
        Control.Monad.when preformOp $ do
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
        preformOp <- checkStackLength (StringOp "parseInteger") 1
        Control.Monad.when preformOp $ do
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
                            
                StackIO e -> do
                    push (StackIO e)
                _ -> do 
                    push $ Error "parseInt can only be used on strings"
                    return ()
                    

    stackWords :: ProgState ()
    stackWords = do
        preformOp <- checkStackLength (StringOp "words") 1
        Control.Monad.when preformOp $ do
            maybeString <- pop
            case maybeString of
                Literal (StackString str) -> do
                    push $ Literal (List $ map (Literal . StackString) (words str))  
                    return ()
                _ -> do
                    push maybeString
                    push $ Error  "could not use function words on non-string type"

    


