module Types where
    import Control.Monad.State
    import qualified Data.Map.Strict as M



    -- data Symbol =
    --     Variable StackLiteral
    --     | Func Ops-- this can only be exec

    data StackLiteral 
         = StackInt    Int
        | StackString String
        | StackFloat   Float
        | StackBool   Bool
        | Variable   String
        | List    [StackLiteral]
        deriving(Show,Ord)

    data Ops
        = Arithmetic String
        | Logical String
        | ControlFlow String
        | StackOp  String
        | ListOp String
        | StackIO String
        | Exec [Ops]
        | Literal StackLiteral
        | Error String
        | AssignmentOp String
        deriving(Show,Eq,Ord)
     
    
    -- data List a = StackLiteral | List1 [List a]
    
    -- data TokenType a = Arithmetic a | Logical a | Literal a | List [a] | Exec a | ControlFlow a | StackOp a | ListOp a | TokenError a deriving(Show)
    data TokenType a = Op a | Literals a | Lists [a] | Execs a | StackOps a | ListOps a | TokenError a deriving(Show)

    type ProgState = State (AssignmentMap, Stack)

    type Map = M.Map StackLiteral 
    type AssignmentMap = Map Ops
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
        -- StackBool a == StackInt b = a == (M.lookup b)


    -- instance Functor (StackLiteral) where 
    --     fmap f (Literal a) = Literal(f a) 


    instance Functor TokenType where  
        fmap f (Literals a) = Literals (f a)  
        fmap f (Execs a) = Literals (f a)  
        -- fmap f (Lis t a) = List (map f a)  

    instance Applicative TokenType where
        pure             =  Literals
        (Literals a) <*> f = fmap a f
        Execs a <*> f = fmap a f
        -- (List a) <*> f = [fmap a f]


    instance Num StackLiteral where
        StackInt a + StackInt b = StackInt (a + b)
        StackFloat a + StackFloat b = StackFloat (a + b)
        StackInt a + StackFloat b = StackFloat ((fromIntegral a :: Float) + b)
        StackFloat a + StackInt b = StackFloat ((fromIntegral b :: Float) + a)

        StackInt a + _  = StackString "error!"
        -------------------- min ---------------------------
        StackInt a - StackInt b = StackInt (a - b)
        StackFloat a - StackFloat b = StackFloat (a - b)
        StackInt a - StackFloat b = StackFloat ((fromIntegral a :: Float) - b)
        StackInt a - _  = StackString "error!"
        ------------------- mult ---------------------------
        StackInt a * StackInt b = StackInt (a * b)
        StackFloat a * StackFloat b = StackFloat (a * b)
        StackInt a * StackFloat b = StackFloat ((fromIntegral a :: Float) * b)
        StackInt a * _  = StackString "error!"


    
    instance Num Ops where
        Literal a + Literal b = Literal (a + b)
        Literal a - Literal b = Literal (a - b)
        Literal a * Literal b = Literal (a * b)
        Literal a * _ = Literal (StackString "error!")


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
