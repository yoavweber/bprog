module Types where
    import Control.Monad.State


    data TokenType a = Arithmetic String | Logical String | Literal String |StackOp String | TokenError String deriving(Show)
    type Stack = [TokenType String]

    type ProgState = State Stack (Stack)
