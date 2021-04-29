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



    parseBool :: Parser StackLiteral 
    parseBool = do
        bool <- try (string "False" <|> string "True")
        return $ StackBool (read bool :: Bool) 


    parseInt :: Parser StackLiteral
    parseInt = do
        first <- many1 digit
        return $  StackInt (read first :: Int)  

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
        op <- try (string "if" <|> string "map" <|> string "foldl" <|> string "times" <|> string "exec")
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
            <|> parseIO
            <|> parseAssignment
            <|> parseArithmetic
            <|> parseStackOp
            <|> parseListOp
            <|> parseBracketsList
            <|> Literal <$> parseLiteral
        

    parseStack = parse(handleSpace <* eof) "bprog parse"
    
