module Parser where
    import Text.Read (readMaybe)
    import Types
    import Data.List
    import Data.List.Split




    slice :: Int -> Int -> [a] -> [a]
    slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

    
    getTokenType :: String -> StackElement
    getTokenType e
        | e == "+" = Arithmetic e
        | e == "*" = Arithmetic e
        | e == "==" = Arithmetic e
        -- | e == "+" = Arithmetic (StackOps e)
        -- | e == "&&" = Logical e
        | (e == "pop" || e == "swap" || e == "dup") = StackOp e
        | checkListOp e == True = ListOp e
        -- | (head e) == '[' =  Literal (List  ( words  (tail $ init e)))
        | (head e) == '[' =  Literal (List  ( map (\t -> case  assignLiteral t of {Nothing -> Variable t;  Just literal ->  literal})$ words  (tail $ init e)))
        -- -- | (head e) == '[' =  List ([e])
        | e == "if" = ControlFlow e 
        | e == "exec" = ControlFlow e 
        | e == ":=" = AssignmentOp e
        | (head e) == '{' = Exec (tail $ init e)
        | checkLiteral e == True = case  assignLiteral e of
            Nothing -> Literal $ Variable e
            Just literal -> Literal literal
        | otherwise = Literal $ Variable e -- TODO: error, handle undefine value
        -- | otherwise = TokenError e

    checkLiteral :: String -> Bool
    checkLiteral e
        | (head e) == '\"' =  True
        | (head e) == '[' =  True
        | (readMaybe e :: Maybe Bool) == Just (read e :: Bool) = True
        | (readMaybe e :: Maybe Float ) == Just (read e :: Float) = True
        | otherwise = False

    -- TODO: very dirty solution, refactor!
    assignLiteral :: String -> Maybe StackLiteral
    assignLiteral e 
        | (head e) == '[' =  Just (List  ( map (\t -> case  assignLiteral t of {Nothing -> Variable t;  Just literal ->  literal}) $ words  (tail $ init e)))
        | (head e) == '\"' = Just (StackString e)

        -- | (head e) == '[' = Just (List $  map (\t -> case  assignLiteral t of {Nothing -> Variable t;  Just literal ->  literal}) $ words (tail $ init e)  ) 
        | (readMaybe e :: Maybe Bool) == Just (read e :: Bool) = Just (StackBool (read e :: Bool) )
        | intOrFloat e == "float" = case readMaybe e :: Maybe Float of
            Nothing -> Nothing-- this is an assignment
            Just n -> Just (StackFloat n)
        | intOrFloat e == "int" = case readMaybe e :: Maybe Int of
            Nothing -> Nothing 
            Just n -> Just(StackInt n)
        | otherwise = Nothing -- this is an assignment



    intOrFloat :: String -> String
    intOrFloat e = case any (=='.') e of
        True -> "float"
        False -> "int"



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

          

    tokenize :: [String] -> [String]
    tokenize list = tokenizeCurlyBrackets $ handleListWrapper $ parseString list 

   
   
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
        -- in (fst $ splitAt startListIndex list  ) ++ [intersperse ' ' $ "[ " ++ (intercalate " " (handleList arrayToken)) ++ " ]"] ++ (tail $ snd $ splitAt endListIndex list)
        in (fst $ splitAt startListIndex list  ) ++ ["[ " ++ (intercalate " " (handleList arrayToken)) ++ " ]"] ++ (tail $ snd $ splitAt endListIndex list)

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