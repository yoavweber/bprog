module Types where
    import Control.Monad.State


    data StackLiteral 
         = StackInt    Int
        | StackString String
        | StackFloat   Float
        | StackBool   Bool
        | List    [String]
        deriving(Show)

    data Ops
        = Arithmetic String
        | Logical String
        | ControlFlow String
        | StackOp  String
        | ListOp String
        | Exec String
        | Literal StackLiteral
        | Error String
        deriving(Show,Eq)
     
    
    -- data List a = StackLiteral | List1 [List a]
    
    -- data TokenType a = Arithmetic a | Logical a | Literal a | List [a] | Exec a | ControlFlow a | StackOp a | ListOp a | TokenError a deriving(Show)
    data TokenType a = Op a | Literals a | Lists [a] | Execs a | StackOps a | ListOps a | TokenError a deriving(Show)

    type Stack = [StackElement]
    type StackElement = Ops

    instance Eq StackLiteral where  
        StackInt a == StackInt b = a == b 
        StackString a == StackString b = a == b  
        StackFloat a == StackFloat b = a == b
        StackBool a == StackBool b = a == b 
        StackInt a == StackFloat b = (fromIntegral a :: Float) == b 
        StackInt a == _ = False 
        StackString a == _ = False
        StackFloat a == _ = False
        StackBool a == _ = False

    instance Functor TokenType where  
        fmap f (Literals a) = Literals (f a)  
        fmap f (Execs a) = Literals (f a)  
        -- fmap f (Lis t a) = List (map f a)  

    instance Applicative TokenType where
        pure             =  Literals
        (Literals a) <*> f = fmap a f
        Execs a <*> f = fmap a f
        -- (List a) <*> f = [fmap a f]

    
    -- instance Show (StackLiteral) where
    --     show (StackInt a) = show a
    --     show (StackString a) =  a
    -- instance (Show a) => Show (TokenType a) where
        -- show (Op a)= show a
        -- show (Branch v l r) = "(left: " ++ show l ++ ") " ++ show v ++ " (right: " ++ show r ++ ")"
    instance Num StackLiteral where
        StackInt a + StackInt b = StackInt (a + b)
        StackInt a + _  = StackString "error!"
    
    instance Num Ops where
        Literal a + Literal b = Literal (a + b)
        -- StackInt a + _  = StackString "error!"


    data ProgramError =
            StackEmpty
        | UnknownSymbol
        | ExpectedBool
        | ExpectedBoolOrNumber
        | ExpectedEnumerable
        | ExpectedQuotation
        | ExpectedList
        | ExpectedVariable
        | DivisionByZero
        | ProgramFinishedWithMultipleValues
        | NumberConversionError
            deriving (Eq, Show)

    -- | Represents parser errors.
    data ParserError =
            IncompleteString
        | IncompleteList
        | IncompleteQuotation
