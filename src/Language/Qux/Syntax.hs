
{-|
Module      : Language.Qux.Syntax
Description : Abstract syntax tree nodes.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Abstract syntax tree nodes.
-}

module Language.Qux.Syntax where


-- | An identifier.
type Id = String

-- * Nodes

-- | A program is a module identifier and a list of declarations.
data Program = Program [Id] [Decl]
    deriving (Eq, Show)

-- | A declaration.
data Decl = FunctionDecl Id [(Type, Id)] [Stmt] -- ^ A name, list of ('Type', 'Id') parameters and statements. The return type is treated as a parameter with id "@".
    deriving (Eq, Show)

name :: Decl -> Id
name (FunctionDecl n _ _) = n

types :: Decl -> [Type]
types (FunctionDecl _ ps _) = map fst ps

parameters :: Decl -> [(Type, Id)]
parameters (FunctionDecl _ ps _) = init ps

parameterNames :: Decl -> [Id]
parameterNames = (map snd) . parameters

parameterTypes :: Decl -> [Type]
parameterTypes = (map fst) . parameters

returnType :: Decl -> Type
returnType (FunctionDecl _ ps _) = fst $ last ps

stmts :: Decl -> [Stmt]
stmts (FunctionDecl _ _ ss) = ss

-- | A statement.
data Stmt   = IfStmt Expr [Stmt] [Stmt] -- ^ A condition, true block and false block of statements.
            | ReturnStmt Expr           -- ^ An expression.
            | WhileStmt Expr [Stmt]     -- ^ A condition and block of statements.
    deriving (Eq, Show)

-- | A complex expression.
data Expr   = ApplicationExpr Id [Expr]     -- ^ A function name to call and the arguments to pass.
            | BinaryExpr BinaryOp Expr Expr -- ^ A binary operation.
            | ListExpr [Expr]               -- ^ A list of expressions.
            | UnaryExpr UnaryOp Expr        -- ^ A unary operation.
            | ValueExpr Value               -- ^ A raw value.
    deriving (Eq, Show)

-- | A binary operator.
data BinaryOp   = Acc
                | Mul | Div | Mod
                | Add | Sub
                | Lt  | Lte | Gt | Gte
                | Eq  | Neq
    deriving (Eq, Show)

-- | A unary operator.
data UnaryOp    = Len
                | Neg
    deriving (Eq, Show)

-- | A value is considered to be in it's normal form.
data Value  = BoolValue Bool    -- ^ A boolean.
            | IntValue Integer  -- ^ An unbounded integer.
            | ListValue [Value] -- ^ A normalised list of values.
            | NilValue          -- ^ A unit value.
    deriving (Eq, Show)

-- | A type.
data Type   = BoolType
            | IntType
            | ListType Type -- ^ A list type with an inner type.
            | NilType
    deriving (Eq, Show)

