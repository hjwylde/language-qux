{-|
Module      : Language.Qux.Annotated.Syntax
Description : Abstract syntax tree nodes with annotations.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

Abstract syntax tree nodes with annotations.
The annotation style was inspired by haskell-src-exts.

Instances of 'Simplifiable' are provided for simplifying a node down to it's unannotated form and of
    'Pretty' for pretty printing.
The instances of 'Eq' are defined in terms of the simplified nodes, i.e., the annotation does not
    impact node equality.
-}

{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Language.Qux.Annotated.Syntax (
    -- * Type classes
    Annotated(..),
    Simplifiable(..),

    -- * Annotated nodes
    Id(..), Program(..), Decl(..), Attribute(..), Stmt(..), Expr(..), Type(..),

    -- * Regular nodes
    BinaryOp(..), UnaryOp(..), Value(..),

    -- * Extra methods

    -- ** Utility
    qualify, mangle,

    -- ** Rendering
    pShow,
) where

import Data.Function
import Data.List
import Data.Tuple.Extra

import           Language.Qux.Syntax (BinaryOp (..), UnaryOp (..), Value (..), pShow)
import qualified Language.Qux.Syntax as Simp

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
    deriving (Functor, Show)

instance Annotated Id where
    ann (Id a _) = a

instance Eq (Id a) where
    (==) = (==) `on` simp

instance Simplifiable (Id a) [Char] where
    simp (Id _ id) = id

instance Pretty (Id a) where
    pPrint = text . simp

-- | A program is a module identifier (list of 'Id''s) and a list of declarations.
data Program a = Program a [Id a] [Decl a]
    deriving (Functor, Show)

instance Annotated Program where
    ann (Program a _ _) = a

instance Eq (Program a) where
    (==) = (==) `on` simp

instance Simplifiable (Program a) Simp.Program where
    simp (Program _ module_ decls) = Simp.Program (map simp module_) (map simp decls)

instance Pretty (Program a) where
    pPrint = pPrint . simp

-- | A declaration.
data Decl a = FunctionDecl a [Attribute a] (Id a) [(Type a, Id a)] [Stmt a] -- ^ A name, list of ('Type', 'Id') parameters and statements.
                                                                            --   The return type is treated as a parameter with id '@'.
            | ImportDecl a [Id a]                                           -- ^ A module identifier to import.
            | TypeDecl a [Attribute a] (Id a)                               -- ^ A type declaration.
    deriving (Functor, Show)

instance Annotated Decl where
    ann (FunctionDecl a _ _ _ _)    = a
    ann (ImportDecl a _)            = a
    ann (TypeDecl a _ _)            = a

instance Eq (Decl a) where
    (==) = (==) `on` simp

instance Simplifiable (Decl a) Simp.Decl where
    simp (FunctionDecl _ attrs name type_ stmts)    = Simp.FunctionDecl (map simp attrs) (simp name) (map (simp *** simp) type_) (map simp stmts)
    simp (ImportDecl _ id)                          = Simp.ImportDecl $ map simp id
    simp (TypeDecl _ attrs name)                    = Simp.TypeDecl (map simp attrs) (simp name)

instance Pretty (Decl a) where
    pPrint = pPrint . simp

-- | A declaration attribute.
data Attribute a = External a
    deriving (Functor, Show)

instance Annotated Attribute where
    ann (External a) = a

instance Eq (Attribute a) where
    (==) = (==) `on` simp

instance Simplifiable (Attribute a) Simp.Attribute where
    simp (External _) = Simp.External

instance Pretty (Attribute a) where
    pPrint = pPrint . simp

-- | A statement.
data Stmt a = IfStmt a (Expr a) [Stmt a] [Stmt a]   -- ^ A condition, true block and false block of statements.
            | ReturnStmt a (Expr a)                 -- ^ An expression.
            | WhileStmt a (Expr a) [Stmt a]         -- ^ A condition and block of statements.
    deriving (Functor, Show)

instance Annotated Stmt where
    ann (IfStmt a _ _ _)    = a
    ann (ReturnStmt a _)    = a
    ann (WhileStmt a _ _)   = a

instance Eq (Stmt a) where
    (==) = (==) `on` simp

instance Simplifiable (Stmt a) Simp.Stmt where
    simp (IfStmt _ condition trueStmts falseStmts)  = Simp.IfStmt (simp condition) (map simp trueStmts) (map simp falseStmts)
    simp (ReturnStmt _ expr)                        = Simp.ReturnStmt (simp expr)
    simp (WhileStmt _ condition stmts)              = Simp.WhileStmt (simp condition) (map simp stmts)

instance Pretty (Stmt a) where
    pPrint = pPrint . simp

-- | A complex expression.
data Expr a = ApplicationExpr a (Id a) [Expr a]         -- ^ A function name (unresolved) to call
                                                        --   and the arguments to pass as parameters.
            | BinaryExpr a BinaryOp (Expr a) (Expr a)   -- ^ A binary operation.
            | CallExpr a [Id a] [Expr a]                -- ^ A function id (resolved) to call and
                                                        --   the arguments to pass as parameters.
            | TypedExpr a Simp.Type (Expr a)            -- ^ A typed expression.
                                                        --   See "Language.Qux.Annotated.TypeResolver".
            | UnaryExpr a UnaryOp (Expr a)              -- ^ A unary operation.
            | ValueExpr a Value                         -- ^ A raw value.
            | VariableExpr a (Id a)                     -- ^ A local variable access.
    deriving (Functor, Show)

instance Annotated Expr where
    ann (ApplicationExpr a _ _) = a
    ann (BinaryExpr a _ _ _)    = a
    ann (CallExpr a _ _)        = a
    ann (TypedExpr a _ _)       = a
    ann (UnaryExpr a _ _)       = a
    ann (ValueExpr a _)         = a
    ann (VariableExpr a _)      = a

instance Eq (Expr a) where
    (==) = (==) `on` simp

instance Simplifiable (Expr a) Simp.Expr where
    simp (ApplicationExpr _ name arguments) = Simp.ApplicationExpr (simp name) (map simp arguments)
    simp (BinaryExpr _ op lhs rhs)          = Simp.BinaryExpr op (simp lhs) (simp rhs)
    simp (CallExpr _ id arguments)          = Simp.CallExpr (map simp id) (map simp arguments)
    simp (TypedExpr _ type_ expr)           = Simp.TypedExpr type_ (simp expr)
    simp (UnaryExpr _ op expr)              = Simp.UnaryExpr op (simp expr)
    simp (ValueExpr _ value)                = Simp.ValueExpr value
    simp (VariableExpr _ name)              = Simp.VariableExpr $ simp name

instance Pretty (Expr a) where
    pPrint = pPrint . simp

-- | A type.
data Type a = AnyType a
            | BoolType a
            | IntType a
            | NilType a
            | StrType a
    deriving (Functor, Show)

instance Annotated Type where
    ann (AnyType a)     = a
    ann (BoolType a)    = a
    ann (IntType a)     = a
    ann (NilType a)     = a
    ann (StrType a)     = a

instance Eq (Type a) where
    (==) = (==) `on` simp

instance Simplifiable (Type a) Simp.Type where
    simp (AnyType _)    = Simp.AnyType
    simp (BoolType _)   = Simp.BoolType
    simp (IntType _)    = Simp.IntType
    simp (NilType _)    = Simp.NilType
    simp (StrType _)    = Simp.StrType

instance Pretty (Type a) where
    pPrint = pPrint . simp

-- | Qualifies the identifier into a single 'Id' joined with periods.
qualify :: [Id a] -> Id a
qualify id = Id (ann $ head id) (intercalate "." (map simp id))

-- | Mangles the identifier into a single 'Id' joined with underscores.
mangle :: [Id a] -> Id a
mangle id = Id (ann $ head id) (intercalate "_" (map simp id))
