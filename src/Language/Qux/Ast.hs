
{-|
Module      : Language.Qux.Ast
Description : Contains the abstract syntax tree elements.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Contains the abstract syntax tree elements.
-}

module Language.Qux.Ast where


-- | An identifier.
type Id = String

-- * Nodes

-- | A program is a list of declarations.
newtype Program = Program [Decl]
    deriving (Eq, Show)

-- | A declaration.
data Decl = FunctionDecl Id [(Type, Id)] [Stmt] -- ^ A name, list of ('Type', 'Id') parameters and statements. The return type is treated as a parameter with id "@".
    deriving (Eq, Show)

-- | A statement.
data Stmt   = IfStmt Expr [Stmt] [Stmt] -- ^ A condition, true block and false block of statements.
            | ReturnStmt Expr           -- ^ An expression
            | WhileStmt Expr [Stmt]     -- ^ A condition and block of statements.
    deriving (Eq, Show)

-- | A complex expression.
data Expr   = ApplicationExpr Id [Expr]     -- ^ A function name to call and the arguments to pass.
            | InfixExpr InfixOp Expr Expr   -- ^ An infix operation.
            | ListExpr [Expr]               -- ^ A list of expressions.
            | ValueExpr Value               -- ^ A raw value.
    deriving (Eq, Show)

-- | An infix operator.
data InfixOp    = Acc
                | Mul | Div | Mod
                | Add | Sub
                | Lt  | Lte | Gt | Gte
                | Eq  | Neq
    deriving (Eq, Show)

-- | A value is considered to be in it's normal form.
data Value  = BoolValue Bool    -- ^ A boolean.
            | IntValue Integer  -- ^ An unbounded integer.
            | ListValue [Value] -- ^ A normalised list of values.
            | NilValue          -- ^ A unit value
    deriving (Eq, Show)

-- | A type
data Type   = BoolType
            | IntType
            | ListType Type -- ^ A list type with an inner type.
            | NilType
    deriving (Eq, Show)

