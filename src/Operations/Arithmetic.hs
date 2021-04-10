module Operations.Arithmetic where
    import Control.Monad.State
    import Types

    -- TODO: try to create a generic function which getting as an input the operation
    -- create a folder for all aritmic operations and move this function
    handleAritmic :: TokenType String -> State Stack ()
    handleAritmic t = case t of
        Arithmetic "+" -> return ()
        otherwise ->  put [Arithmetic "1"]


        
    -- opAdd :: State Stack ()
    -- opAdd = do
    --     a <- pop
    --     b <- pop
    --     -- poping the plus sign
    --     pop
    --     let res =  ((++) <$> a <*> b) 
    --     push res
    --     return()



        
    -- opAdd :: State Stack ()
    -- opAdd = do
    --     a <- pop
    --     b <- pop
    --     -- poping the plus sign
    --     pop
    --     let res =  ((++) <$> a <*> b) 
    --     push res
    --     return()

