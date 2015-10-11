
{-|
Module      : Language.Qux.Annotated.Syntax
Description : Abstract syntax tree nodes with annotations.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Abstract syntax tree nodes with annotations.
The annotation style was inspired by haskell-src-exts.

Instances of 'Simplifiable' are provided for simplifying a node down to it's unannotated form and of
    'Pretty' for pretty printing.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Qux.Annotated.Syntax (
    -- * Type classes
    Annotated(..),
    Simplifiable(..),

    -- * Annotated nodes
    Id(..), Program(..), Decl(..), Stmt(..), Expr(..), Type(..),

    -- * Regular nodes
    BinaryOp(..), UnaryOp(..), Value(..)
) where

import              Language.Qux.Syntax (BinaryOp(..), UnaryOp(..), Value(..))
import qualified    Language.Qux.Syntax as S

import Text.PrettyPrint.HughesPJClass


-- | An annotated class.
--   Annotations are used for attaching data to a node, such as a 'Text.Parsec.SourcePos'.
class Annotated n where
    ann :: n a -> a

-- | A simplifiable class.
--   Simplifiable is used to simplify a node to a a simpler form.
--   See "Language.Qux.Syntax" for simpler forms of the nodes defined here.
class Simplifiable n r | n -> r where
    simp :: n -> r


-- | An identifier. Identifiers should match '[a-z_][a-zA-Z0-9_']*'.
data Id a = Id a String
    deriving (Eq, Functor, Show)

instance Annotated Id where
    ann (Id a _) = a

instance Simplifiable (Id a) [Char] where
    simp (Id _ id) = id

instance Pretty (Id a) where
    pPrint = text . simp


-- | A program is a module identifier (list of 'Id''s) and a list of declarations.
data Program a = Program a [Id a] [Decl a]
    deriving (Eq, Functor, Show)

instance Annotated Program where
    ann (Program a _ _) = a

instance Simplifiable (Program a) S.Program where
    simp (Program _ module_ decls) = S.Program (map simp module_) (map simp decls)

instance Pretty (Program a) where
    pPrint = pPrint . simp


-- | A declaration.
data Decl a = FunctionDecl a (Id a) [(Type a, Id a)] [Stmt a]   -- ^ A name, list of ('Type', 'Id') parameters and statements.
                                                                --   The return type is treated as a parameter with id '@'.
            | ImportDecl a [Id a]                               -- ^ A module identifier to import.
    deriving (Eq, Functor, Show)

instance Annotated Decl where
    ann (FunctionDecl a _ _ _)  = a
    ann (ImportDecl a _)        = a

instance Simplifiable (Decl a) S.Decl where
    simp (FunctionDecl _ name type_ stmts) = S.FunctionDecl (simp name) (map (tmap simp simp) type_) (map simp stmts)
    simp (ImportDecl _ id) = S.ImportDecl $ map simp id

instance Pretty (Decl a) where
    pPrint = pPrint . simp


-- | A statement.
data Stmt a = IfStmt a (Expr a) [Stmt a] [Stmt a]   -- ^ A condition, true block and false block of statements.
            | ReturnStmt a (Expr a)                 -- ^ An expression.
            | WhileStmt a (Expr a) [Stmt a]         -- ^ A condition and block of statements.
    deriving (Eq, Functor, Show)

instance Annotated Stmt where
    ann (IfStmt a _ _ _)    = a
    ann (ReturnStmt a _)    = a
    ann (WhileStmt a _ _)   = a

instance Simplifiable (Stmt a) S.Stmt where
    simp (IfStmt _ condition trueStmts falseStmts)  = S.IfStmt (simp condition) (map simp trueStmts) (map simp falseStmts)
    simp (ReturnStmt _ expr)                        = S.ReturnStmt (simp expr)
    simp (WhileStmt _ condition stmts)              = S.WhileStmt (simp condition) (map simp stmts)

instance Pretty (Stmt a) where
    pPrint = pPrint . simp


-- | A complex expression.
data Expr a = ApplicationExpr a (Id a) [Expr a]         -- ^ A function name (unresolved) to call
                                                        --   and the arguments to pass as parameters.
            | BinaryExpr a BinaryOp (Expr a) (Expr a)   -- ^ A binary operation.
            | CallExpr a [Id a] [Expr a]                -- ^ A function id (resolved) to call and
                                                        --   the arguments to pass as parameters.
            | ListExpr a [Expr a]                       -- ^ A list expression.
            | TypedExpr a S.Type (Expr a)               -- ^ A typed expression.
                                                        --   See "Language.Qux.Annotated.TypeResolver".
            | UnaryExpr a UnaryOp (Expr a)              -- ^ A unary operation.
            | ValueExpr a Value                         -- ^ A raw value.
            | VariableExpr a (Id a)                     -- ^ A local variable access.
    deriving (Eq, Functor, Show)

instance Annotated Expr where
    ann (ApplicationExpr a _ _) = a
    ann (BinaryExpr a _ _ _)    = a
    ann (CallExpr a _ _)        = a
    ann (ListExpr a _)          = a
    ann (TypedExpr a _ _)       = a
    ann (UnaryExpr a _ _)       = a
    ann (ValueExpr a _)         = a
    ann (VariableExpr a _)      = a

instance Simplifiable (Expr a) S.Expr where
    simp (ApplicationExpr _ name arguments) = S.ApplicationExpr (simp name) (map simp arguments)
    simp (BinaryExpr _ op lhs rhs)          = S.BinaryExpr op (simp lhs) (simp rhs)
    simp (CallExpr _ id arguments)          = S.CallExpr (map simp id) (map simp arguments)
    simp (ListExpr _ elements)              = S.ListExpr (map simp elements)
    simp (TypedExpr _ type_ expr)           = S.TypedExpr type_ (simp expr)
    simp (UnaryExpr _ op expr)              = S.UnaryExpr op (simp expr)
    simp (ValueExpr _ value)                = S.ValueExpr value
    simp (VariableExpr _ name)              = S.VariableExpr $ simp name

instance Pretty (Expr a) where
    pPrint = pPrint . simp


-- | A type.
data Type a = BoolType a
            | IntType a
            | ListType a (Type a) -- ^ A list type with an inner type.
            | NilType a
    deriving (Eq, Functor, Show)

instance Annotated Type where
    ann (BoolType a)    = a
    ann (IntType a)     = a
    ann (ListType a _)  = a
    ann (NilType a)     = a

instance Simplifiable (Type a) S.Type where
    simp (BoolType _)       = S.BoolType
    simp (IntType _)        = S.IntType
    simp (ListType _ inner) = S.ListType $ simp inner
    simp (NilType _)        = S.NilType

instance Pretty (Type a) where
    pPrint = pPrint . simp


-- Helper methods

tmap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
tmap f g (a, c) = (f a, g c)

