
{-|
Module      : Language.Qux.Annotated.Syntax
Description : Abstract syntax tree nodes with annotations.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Abstract syntax tree nodes with annotations.

The annotation style was inspired by haskell-src-exts.
-}

module Language.Qux.Annotated.Syntax (
    -- * Type class
    Annotated(..),

    -- * Annotated nodes
    Id(..), Program(..),

    Decl(..),
    name, types, parameters, parameterNames, parameterTypes, returnType, stmts,

    Stmt(..), Expr(..), Type(..),

    -- * Regular nodes
    BinaryOp(..), UnaryOp(..), Value(..)
) where

import Language.Qux.Syntax (BinaryOp(..), UnaryOp(..), Value(..))


-- | An annotated class.
class Annotated node where
    ann :: node a -> a


-- | An identifier.
data Id a = Id a String
    deriving (Eq, Show)

instance Annotated Id where
    ann (Id a _) = a

-- | A program is a list of declarations.
data Program a = Program a [Decl a]
    deriving (Eq, Show)

instance Annotated Program where
    ann (Program a _) = a

-- | A declaration.
data Decl a = FunctionDecl a (Id a) [(Type a, Id a)] [Stmt a] -- ^ A name, list of ('Type', 'Id') parameters and statements. The return type is treated as a parameter with id "@".
    deriving (Eq, Show)

instance Annotated Decl where
    ann (FunctionDecl a _ _ _) = a

name :: Decl a -> Id a
name (FunctionDecl _ n _ _) = n

types :: Decl a -> [Type a]
types (FunctionDecl _ _ ps _) = map fst ps

parameters :: Decl a -> [(Type a, Id a)]
parameters (FunctionDecl _ _ ps _) = init ps

parameterNames :: Decl a -> [Id a]
parameterNames = (map snd) . parameters

parameterTypes :: Decl a -> [Type a]
parameterTypes = (map fst) . parameters

returnType :: Decl a -> Type a
returnType (FunctionDecl _ _ ps _) = fst $ last ps

stmts :: Decl a -> [Stmt a]
stmts (FunctionDecl _ _ _ ss) = ss

-- | A statement.
data Stmt a = IfStmt a (Expr a) [Stmt a] [Stmt a]   -- ^ A condition, true block and false block of statements.
            | ReturnStmt a (Expr a)                 -- ^ An expression.
            | WhileStmt a (Expr a) [Stmt a]         -- ^ A condition and block of statements.
    deriving (Eq, Show)

instance Annotated Stmt where
    ann (IfStmt a _ _ _)    = a
    ann (ReturnStmt a _)    = a
    ann (WhileStmt a _ _)   = a

-- | A complex expression.
data Expr a = ApplicationExpr a (Id a) [Expr a]         -- ^ A function name to call and the arguments to pass.
            | BinaryExpr a BinaryOp (Expr a) (Expr a)   -- ^ A binary operation.
            | ListExpr a [Expr a]                       -- ^ A list of expressions.
            | UnaryExpr a UnaryOp (Expr a)              -- ^ A unary operation.
            | ValueExpr a Value                         -- ^ A raw value.
    deriving (Eq, Show)

instance Annotated Expr where
    ann (ApplicationExpr a _ _) = a
    ann (BinaryExpr a _ _ _)    = a
    ann (ListExpr a _)          = a
    ann (UnaryExpr a _ _)       = a
    ann (ValueExpr a _)         = a

-- | A type.
data Type a = BoolType a
            | IntType a
            | ListType a (Type a) -- ^ A list type with an inner type.
            | NilType a
    deriving (Eq, Show)

instance Annotated Type where
    ann (BoolType a)    = a
    ann (IntType a)     = a
    ann (ListType a _)  = a
    ann (NilType a)     = a

