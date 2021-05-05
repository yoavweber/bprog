module Parser where
    import Text.Read (readMaybe)
    import Types
    import Data.List
    import Data.Char(isSpace)

    import Control.Monad
    import Numeric

    import Text.ParserCombinators.Parsec hiding (spaces)



    trim :: [Char] -> [Char]
    trim = dropWhileEnd isSpace . dropWhile isSpace

    symbol :: Parser Char
    symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

    ------- parser
    parseLine :: String -> Stack 
    parseLine input = case parseStack input of
        Left err -> [Error ("Prasing error: " ++ show err)]
        Right val ->  val

    spaces:: Parser()
    spaces = skipMany space

    -- check that the verible is not any of the word of the other parser
    -- if its not, allow it as a varible
    parseVarible :: Parser StackElement 
    parseVarible = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let var = first:rest
        return  (Literal (Variable var))
    

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
        n <- try (pPos <|> pNeg )
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

    parseBool :: Parser StackLiteral 
    parseBool = do
        bool <- try (string "False" <|> string "True")
        return $ StackBool (read bool :: Bool) 

    handleList :: Parser Stack
    handleList =  endBy (parseAll <?> "") spaces

    parseList :: Parser StackLiteral
    parseList = do
        spaces
        char '[' 
        char ' ' 
        x <- try handleList
        char ']'
        spaces
        return (List x)


    handleBracketsList :: Parser Stack
    handleBracketsList = endBy (parseAll <?> "error parsing curly brackets") spaces

    parseBracketsList :: Parser StackElement   
    parseBracketsList = do
        spaces
        char '{' 
        char ' ' 
        x <- try handleBracketsList
        char '}'
        spaces
        return (Exec x)


    parseIf :: Parser StackElement 
    parseIf = do 
        op <- try (string "if" <|>  string "exec" <|> string "loop")
        return (ControlFlow op)

    parseListOps :: Parser StackElement 
    parseListOps = do
        op <- try (string "map" <|> string "foldl" <|> string "times" <|> string "each" )
        return (ControlFlow op)

    parseAssignment :: Parser StackElement 
    parseAssignment = do
        op <- try (string ":=" <|> string "fun" )
        return (AssignmentOp op)
    

    parseStackOp :: Parser StackElement 
    parseStackOp = do 
        op <- try (string "pop" <|> string "swap" <|> string "dup")
        return (StackOp op)

    parseListOp :: Parser StackElement  
    parseListOp = do
        op <- try (string "head" <|> string "tail" <|> string "empty" <|> string "length" <|> string "cons" <|> string "append")
        return (ListOp op)

    stringParsing :: Parser StackElement 
    stringParsing =  parseStringFloat <|> parseStringInt <|> parseWords
       

    parseWords :: Parser StackElement 
    parseWords = do
        op <- try (string "words" )
        return $ StringOp op


    parseStringFloat :: Parser StackElement 
    parseStringFloat = do
        op <- try (string "parseFloat" )
        return $ StringOp op

    parseStringInt :: Parser StackElement 
    parseStringInt = do
        op <- try (string "parseInteger" <|> string "words")
        return $ StringOp op

    parseArithmetic :: Parser StackElement 
    parseArithmetic = do
        op <- try (string "+" <|> string "-" <|> string "*" <|> string "==" <|> string "div" <|> string "/" <|> string "<" <|> string ">" <|> string "not" <|> string "||" <|> string "&&")
        return (Arithmetic op)

    parseIO :: Parser StackElement 
    parseIO = do
        op <- try (string "print" <|> string "read")
        return (StackIO op)


    parseLiteral :: Parser StackElement 
    parseLiteral =  Literal <$> parseString 
                <|> Literal <$> parseBool
                <|> Literal <$> try parseFloat
                <|> Literal <$> parseInt
                <|> Literal <$> parseList

    handleSpace = do
        endBy parseAll spaces

    parseAll = parseIf
            <|> parseLiteral
            <|> parseListOps
            <|> stringParsing
            <|> parseIO
            <|> parseAssignment
            <|> parseArithmetic
            <|> parseStackOp
            <|> parseListOp
            <|> parseBracketsList
            <|> parseVarible
    
        

    parseStack = parse(handleSpace <* eof) "bprog parse"
    
