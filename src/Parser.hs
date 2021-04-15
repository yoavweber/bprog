module Parser where
    import Text.Read (readMaybe)
    import Types
    import Data.List
    import Data.List.Split


-- Instead of type string change it to type stack 


-- TODO: change naming of function
-- create 1 token from list/string or function expersion
-- parseString :: [String] -> String -> [String]
-- parseString list = case (any (=="\"") list) of
--     True -> parseString ((takeWhile (/= "\"") list) ++ (stringefy $ tail $ dropWhile (/= "\"") list ))
--     False -> list


-- stringefy :: [String] -> [String]
-- stringefy list  =
--     let parseStringWrapper ("\"":xs) str  =  (init(str)):xs
--         parseStringWrapper (x:xs) str = parseStringWrapper xs (str ++ x ++ " ")  
--     in
--         parseStringWrapper list ""


-- parseBrackets :: [String] -> String -> [String]
-- parseBrackets list openBrackets closeBrackets = case (any (==t) list) of
--     True -> parseBrackets ((takeWhile (/= openBrackets) list) ++ (parseString $ tail $ dropWhile (/= closeBrackets) list )) openBrackets closeBrackets
--     False -> list

-- Create an expection in case there would be no closing token
-- checkTokens:: [String] ->  [String]
-- checkTokens list = do
--     let makeString = parseString list "\""
--     return makeString
    slice :: Int -> Int -> [a] -> [a]
    slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

    getTokenType :: String -> StackElement
    getTokenType e
        | e == "+" = Arithmetics e
        -- | e == "+" = Arithmetic (StackOps e)
        -- | e == "&&" = Logical e
        -- | (e == "pop" || e == "swap" || e == "dup") = StackOp e
        -- | checkListOp e == True = ListOp e
        -- -- | (head e) == '[' =  List ( words  (tail $ init e))
        -- -- | (head e) == '[' =  List ([e])
        -- | e == "exec" = ControlFlow e 
        -- | (head e) == '{' = Exec (tail $ init e)
        -- | checkLiteral e == True = Literal e
        -- | otherwise = TokenError e

    checkLiteral :: String -> Bool
    checkLiteral e
        | (head e) == '\"' =  True
        -- | (head e) == '[' =  True
        | (readMaybe e :: Maybe Float ) == Just (read e :: Float) = True
        | otherwise = False

    -- Map and function didn't impelemented yet
    checkListOp :: String -> Bool
    checkListOp e
        | e == "head" = True
        | e == "tail" = True
        | e == "empty" = True
        | e == "length"= True
        | e == "cons" = True
        | e == "append" = True
        | otherwise = False

          


    -- use elemIndices to plit the list
    -- tokenizeList :: [String] ->  [String]
    -- tokenizeList list = do
    --     case (any (=="[") list) of
    --         True -> tokenizeList ((takeWhile (/= "[") list) ++ (parseList $ tail $ dropWhile (/= "[") list ))
    --         False -> list


    tokenize :: [String] -> [String]
    tokenize list = tokenizeCurlyBrackets $ handleListWrapper $ parseString list 

   
   
    -- parseList :: [String] -> [String]
    -- parseList list =
    --     let parseListWrapper ("]":xs) str  =  ("[" ++ (init(str)) ++ "]"):xs
    --         parseListWrapper (x:xs) str = case x of 
    --             "[" -> parseList xs
    --             otherwise -> parseListWrapper xs (str ++ x ++ ",") 
    --     in
    --         parseListWrapper list ""


    
            

    parseString :: [String] ->  [String]
    parseString list  = do
        let quotesIndices = "\"" `elemIndices` list
        case (length quotesIndices == 0) of
            True -> list -- meaning there are no strings in the experssion
            False -> case ( even $ length quotesIndices) of
                False -> ["error"] -- TODO: create parsing error
                True -> unwordStrings quotesIndices list



    unwordStrings :: [Int] -> [String] -> [String]
    unwordStrings [] list = list
    unwordStrings (x:s:_) list = do
        let newList =  (fst $ splitAt x list) ++ ([unwords $ slice (x+1) s list]) ++ (snd $ splitAt (s+1) list)
        let xs = "\"" `elemIndices` newList
        unwordStrings xs newList


    -- TODO: parse bracktes and list as one
    tokenizeCurlyBrackets :: [String] ->  [String]
    tokenizeCurlyBrackets list = do
        case (any (=="{") list) of
            True -> tokenizeCurlyBrackets ((takeWhile (/= "{") list) ++ (parseCurlyBrackets $ tail $ dropWhile (/= "{") list ))
            False -> list



    parseCurlyBrackets :: [String] -> [String]
    parseCurlyBrackets list =
        let parseListWrapper ("}":xs) str  =  ("{" ++ (init(str)) ++ "}"):xs
            parseListWrapper (x:xs) str = case x of 
                "{" -> parseCurlyBrackets xs
                otherwise -> parseListWrapper xs (str ++ x ++ " ") 
        in
            parseListWrapper list ""



--  -- use elemIndices to plit the list
--     tokenize :: [String] ->  String -> String -> [String]
--     tokenize list open close = do
--         -- let openBrackets = open `elemIndices` list
--         -- let closeBrackets = close `elemIndices` list
        
--         case (any (==open) list) of
--             True -> tokenize ((takeWhile (/= open) list) ++ (parseList (tail $ dropWhile (/= open) list) open close )) open close
--             False -> list


--     parseList :: [String] -> String -> String -> [String]
--     parseList list open close =
--         let parseListWrapper (close:xs) str  =  (open ++ (init(str)) ++ close):xs
--             parseListWrapper (x:xs) str = do 
--                 if x == open 
--                     then parseList xs open close
--                     else   parseListWrapper xs (str ++ x ++ ",") 
--         in
--             parseListWrapper list ""


    -- tokenizeString :: [String] -> [String]
    -- tokenizeString list = do
    --     case (any (=="\"") list) of
    --         True -> tokenizeList ((takeWhile (/= "\"") list) ++ (parseString $ tail $ dropWhile (/= "\"") list ))
    --         False -> list

    -- parseString :: [String] -> [String]
    -- parseString list =
    --     let parseListWrapper [] "" = []
    --         parseListWrapper ("\"":xs) str  =   (init(str)):xs
    --         parseListWrapper (x:xs) str = parseListWrapper xs (str ++ x ++ " ")  
    --     in
    --         parseListWrapper list ""

    -- handleList :: [String] -> [String]
    -- handleList list = case (any (=="[") list ) of
    --     True -> do
    --         let maybeTokenize = parseList list
    --         handleList maybeTokenize
    --     False -> filter (\t ->( (t /= "")) ) list


    -- parseList :: [String] -> [String]
    -- parseList list  =
    --     let parseListWrapper [] str = []
    --         parseListWrapper ("]":xs) ""  = []
    --         parseListWrapper ("]":xs) str  =  [("[" ++ (init(str)) ++ "]")]
    --         parseListWrapper (x:xs) str = case x of 
    --             "[" ->  (splitOn "," str) ++ parseListWrapper xs ("")  ++  ( parseListWrapper ( tail $  (dropWhile (/="]") xs) ++ [""] ) "")
    --             otherwise -> parseListWrapper xs (str ++ x ++ ",")
    --     in
    --             parseListWrapper list ""




    handleListWrapper :: [String] -> [String]
    handleListWrapper list = case (length $ "[" `elemIndices` list) == (length $ "]" `elemIndices` list) of
        False -> ["false"] -- return error, use maybe string
        True -> let openBrackets = "[" `elemIndices` list
                in if openBrackets /= []
                    then concatListElement (head openBrackets) (last  ("]" `elemIndices` list)) list
                    else list
    


    concatListElement :: Int -> Int -> [String] -> [String]
    concatListElement startListIndex endListIndex list =
        let arrayToken = slice (startListIndex + 1) endListIndex list
        in (fst $ splitAt startListIndex list  ) ++ ["[ " ++ (intercalate " " (handleList arrayToken)) ++ "]"] ++ (tail $ snd $ splitAt endListIndex list)

    handleList :: [String] -> [String]
    handleList list = case (any (=="[") list ) of
        True -> 
            let splitList = (span (/="[") list)
            in handleList $ (fst(splitList) ++  (parseList $ snd splitList))
        False -> 
            let removeWhiteSpace = filter (\t ->( (t /= "")) ) list
            in filter (\t ->( (t /= "]")) ) removeWhiteSpace


    parseList :: [String] -> [String]
    parseList list  =
        let parseListWrapper [] str = [str]
            parseListWrapper ("]":xs) ""  = []
            parseListWrapper ("]":xs) str  =  ( "[" ++ (init(str)) ++ "]"):xs
            parseListWrapper ("[":xs) str = (splitOn "," str) ++ parseListWrapper xs ("")  
            parseListWrapper (x:xs) str = parseListWrapper xs (str ++ x ++ ",")

        in
                parseListWrapper list ""