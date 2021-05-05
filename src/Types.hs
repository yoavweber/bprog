module Types where
    import Control.Monad.State
    import qualified Data.Map.Strict as M




    data StackLiteral
         = StackInt    Integer
        | StackString String
        | StackFloat   Float
        | StackBool   Bool
        | Variable   String
        | List    [Ops]
        -- deriving(Show,Ord, Eq)
        deriving(Ord,Eq )

    data Ops
        = Arithmetic String
        | Logical String
        | ControlFlow String
        | StackOp  String
        | ListOp String
        | StackIO String
        | Exec [Ops]
        | StringOp String
        | Literal StackLiteral
        | Error String
        | AssignmentOp String
        deriving(Eq,Ord)
        -- deriving(Show,Eq,Ord )

    instance Show StackLiteral where
        show (StackInt a) = show a
        show (StackFloat a) = show a
        show (StackBool a ) = show a
        show (Variable a) = a
        show (StackString a ) = show a
        show (List a) = show a

    instance Show Ops where
        show (Literal (StackInt a)) = show a
        show (Literal (StackFloat a)) = show a
        show (Literal (StackBool a )) = show a
        show (Literal (List a)) = show a
        show (Literal (Variable a)) = a
        show (Literal (StackString a)) = show a
        show (StackIO a) =  a
        show (Arithmetic a) = a
        show (Logical a) =  a
        show (ControlFlow a) =  a
        show (StackOp a) =  a
        show (ListOp a) =  a
        show (StringOp a) = a
        show (Exec a) =   "{ " ++ unwords (map show a) ++ " }"
        show (Error a) = a



    type ProgState = State (AssignmentMap, Stack)

    type Map = M.Map StackLiteral
    type AssignmentMap = Map Ops
    type Stack = [StackElement]
    type StackElement = Ops

