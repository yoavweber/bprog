module Parser where
    import Text.Read (readMaybe)
    import Types


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

    getTokeType :: String -> TokenType a
    getTokeType e
        | e == "+" = Arithmetic e
        | e == "&&" = Logical e
        | (e == "pop" || e == "swap" || e == "dup") = StackOp e
        | (head e) == '[' =  List ( words  (tail $ init e))
        -- | (head e) == '[' =  List ([e])
        | checkLiteral e == True = Literal e
        | otherwise = TokenError e

    checkLiteral :: String -> Bool
    checkLiteral e
        | (head e) == '\"' =  True
        | (head e) == '[' =  True
        | (readMaybe e :: Maybe Float ) == Just (read e :: Float) = True
        | otherwise = False

    checkListOp :: String -> Bool
    checkListOp e
        | e == "head" = True
        | e == "tail" = True
        | e == "empty" = True
        | e == "length"= True
        | e == "cons" = True
        | e == "append" = True
        | otherwise = False

    tokenize :: [String] ->  String -> String -> [String]
    tokenize list open close = case (any (==open) list) of
        True -> tokenize ((takeWhile (/= open) list) ++ (parseList $ tail $ dropWhile (/= open) list )) open close
        False -> list


    parseList :: [String] -> [String]
    parseList list =
        let parseListWrapper ("]":xs) str  =  ("[" ++ (init(str)) ++ "]"):xs
            parseListWrapper (x:xs) str = case x of 
                "[" -> parseList xs
                otherwise -> parseListWrapper xs (str ++ x ++ ",") 
        in
            parseListWrapper list ""


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