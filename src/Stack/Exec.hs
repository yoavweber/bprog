module Stack.Exec() where 
--     import Parser
--     import Types
    
--     unWrap :: Ops -> String
--     unWrap a = case a of
--         -- Literal t -> t 
--         Exec t -> t
--         -- otherwise ->

--     executeCodeLine :: String -> (AssignmentMap, Stack) -> Stack
--     executeCodeLine line (varMap,previousStack) = 
--         let stack =  (map (\e -> getTokenType e) $ tokenize (words line)) ++ previousStack
--         in snd (execState stackManip (varMap, stack))