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
        | checkLiteral e == True = Literal e
        | otherwise = TokenError e

    checkLiteral :: String -> Bool
    checkLiteral e
        | (head e) == '\"' =  True
        | (head e) == '[' =  True
        | (readMaybe e :: Maybe Float ) == Just (read e :: Float) = True
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
