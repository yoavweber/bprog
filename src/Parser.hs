module Parser where
    import Text.Read (readMaybe)
    import Types
    import Data.List

    import Control.Monad
    import Numeric

    -- import Data.List.Split
    import Text.ParserCombinators.Parsec hiding (spaces)





    -- slice :: Int -> Int -> [a] -> [a]
    -- slice start stop xs = fst $ splitAt (stop - start) (snd $ splitAt start xs)

    trim :: [Char] -> [Char]
    trim = dropWhileEnd isSpace . dropWhile isSpace

    symbol :: Parser Char
    symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

    parseLine :: String -> Stack 
    parseLine input = case parseStack input of
        Left err -> [Error ("No match: " ++ show err)]
        Right val ->  val

    spaces:: Parser()
    spaces = skipMany space

    parseString :: Parser StackLiteral
    parseString = do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $  StackString x


    parseVarible :: Parser StackLiteral
    parseVarible = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let var = first:rest
        return $ Variable var 

    parseFloat :: Parser StackLiteral
    parseFloat = do
        first <- many1 digit
        char '.'
        rest <- many1 digit
        let float = first ++ ('.':rest)
        return $ StackFloat (read float :: Float)  

    parseString :: Parser StackLiteral
    parseString = do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ StackString (trim x)

    
    pPos :: Parser Integer
    pPos = do 
        s <- many1 digit 
        return $ read s

    -- Parse negative number 
    pNeg :: Parser Integer
    pNeg = do
        char '-'
        n <- pPos
        return (-n)


    parseInt :: Parser StackLiteral
    parseInt = do
        n <- try pPos <|> pNeg
        return $  StackInt n  

    pNegFloat :: Parser Float 
    pNegFloat = do
        char '-'
        n <- pFloat
        return (-n)


    pFloat :: Parser Float
    pFloat = do 
        first <- many1 digit
        char '.'
        rest <- many1 digit
        let float = first ++ ('.':rest)
        return (read float :: Float)

    parseFloat :: Parser StackLiteral
    parseFloat = do
        n <- pFloat <|> pNegFloat
        return $ StackFloat n

    parseNumber :: Parser Ops
    parseNumber = liftM (Literal . StackInt . read) $ many1 digit

    -- handleList :: Parser StackLiteral
    handleList =  endBy parseLiteral spaces

    -- TODO: rise an error to stack
    parseList :: Parser StackLiteral
    parseList = do
        char '[' 
        char ' ' 
        x <- try handleList
        char ']'
        return (List x)


    -- handleBracketsList :: Parser Stack
    handleBracketsList = endBy parseAll spaces


    parseBracketsList :: Parser Ops  
    parseBracketsList = do
        char '{' 
        char ' ' 
        x <- try handleBracketsList
        char '}'
        return (Exec x)


    parseIf :: Parser Ops
    parseIf = do 
        op <- try (string "if" <|>  string "exec" <|> string "loop")
        return (ControlFlow op)

    parseListOps = do
        op <- try (string "map" <|> string "foldl" <|> string "times" <|> string "each" )
        return (ControlFlow op)

    parseAssignment :: Parser Ops
    parseAssignment = do
        op <- try (string ":=" <|> string "fun" )
        return (AssignmentOp op)
    

    parseStackOp :: Parser Ops
    parseStackOp = do 
        op <- try (string "pop" <|> string "swap" <|> string "dup")
        return (StackOp op)

    parseListOp :: Parser Ops 
    parseListOp = do
        op <- try (string "head" <|> string "tail" <|> string "empty" <|> string "length" <|> string "cons" <|> string "append")
        return (ListOp op)

    stringParsing :: Parser Ops
    stringParsing =  parseStringFloat <|> parseStringInt <|> parseWords
       

        -- TODO: change aritmic type
        -- return $ Arithmetic op
    parseWords :: Parser Ops
    parseWords = do
        op <- try (string "words" )
        -- TODO: change aritmic type
        return $ Arithmetic op


    parseStringFloat :: Parser Ops
    parseStringFloat = do
        op <- try (string "parseFloat" )
        -- TODO: change aritmic type
        return $ Arithmetic op

    parseStringInt :: Parser Ops
    parseStringInt = do
        op <- try (string "parseInteger" <|> string "words")
        -- TODO: change aritmic type
        return $ Arithmetic op

    parseArithmetic :: Parser Ops
    parseArithmetic = do
        op <- string "+" <|> string "-" <|> string "*" <|> string "==" <|> string "div" <|> string "/"
        return (Arithmetic op)

    parseIO :: Parser Ops
    parseIO = do
        op <- string "print" 
        return (StackIO op)

    parseLiteral :: Parser StackLiteral
    parseLiteral =  parseString 
                <|> parseBool
                <|> try parseFloat 
                <|> parseInt
                <|> parseList
                <|> parseVarible

    handleSpace = do
        endBy parseAll spaces

    parseAll = parseIf
            <|> stringParsing
            <|> parseIO
            <|> parseAssignment
            <|> parseArithmetic
            <|> parseStackOp
            <|> parseListOp
            <|> parseBracketsList
            <|> Literal <$> parseLiteral
        

    parseStack = parse(handleSpace <* eof) "bprog parse"
    
