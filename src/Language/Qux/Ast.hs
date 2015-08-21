
module Language.Qux.Ast where


type Id = String

newtype Program = Program [Decl]
    deriving (Eq, Show)

data Decl = FunctionDecl Id [(Type, Id)] [Stmt]
    deriving (Eq, Show)

data Stmt   = IfStmt Expr [Stmt] [Stmt]
            | ReturnStmt Expr
            | WhileStmt Expr [Stmt]
    deriving (Eq, Show)

data Expr   = ApplicationExpr Id [Expr]
            | InfixExpr InfixOp Expr Expr
            | ListExpr [Expr]
            | ValueExpr Value
    deriving (Eq, Show)

data InfixOp    = Mul | Div
                | Add | Sub
                | Lt | Lte | Gt | Gte
                | Eq | Neq
    deriving (Eq, Show)

data Value  = BoolValue Bool
            | IntValue Integer
            | ListValue [Value]
            | NilValue
    deriving (Eq, Show)

data Type   = BoolType
            | IntType
            | ListType Type
            | NilType
    deriving (Eq, Show)

