module Types where
    import Control.Monad.State


    data StackLiteral 
         = StackInt    Int
        | StackString String
        | StackFloat   Float
        | StackBool   Bool
        | Lists     [StackLiteral]
        deriving(Show)

    data Ops
        = Arithmetics String
        | Logicals String
        | ControlFlows String
        | StackOps  String
        | Literals StackLiteral
        deriving(Show)
     
    
    -- data List a = StackLiteral | List1 [List a]
    
    -- data TokenType a = Arithmetic a | Logical a | Literal a | List [a] | Exec a | ControlFlow a | StackOp a | ListOp a | TokenError a deriving(Show)
    data TokenType a = Op a | Literal a | List [a] | Exec a | StackOp a | ListOp a | TokenError a deriving(Show)

    type Stack = [StackElement]
    type StackElement = Ops

    instance Functor TokenType where  
        fmap f (Literal a) = Literal (f a)  
        fmap f (Exec a) = Literal (f a)  
        -- fmap f (Lis t a) = List (map f a)  

    instance Applicative TokenType where
        pure             =  Literal
        (Literal a) <*> f = fmap a f
        Exec a <*> f = fmap a f
        -- (List a) <*> f = [fmap a f]

    
    -- instance Show (StackLiteral) where
    --     show (StackInt a) = show a
    --     show (StackString a) =  a
    -- instance (Show a) => Show (TokenType a) where
        -- show (Op a)= show a
        -- show (Branch v l r) = "(left: " ++ show l ++ ") " ++ show v ++ " (right: " ++ show r ++ ")"


    type ProgState = State Stack (Stack)


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
