
{-|
Module      : Language.Qux.Annotated.TypeResolver
Description : Type resolving functions that transform the abstract syntax tree to a typed one.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Type resolving functions that transform the abstract syntax tree to a typed one.

These functions will transform every 'Ann.Expr' into an 'Ann.TypedExpr' and return the transformed
    tree.
The "Language.Qux.Annotated.TypeChecker" and "Language.Qux.Llvm.Compiler" modules require the tree
    to be typed.
-}

module Language.Qux.Annotated.TypeResolver (
    -- * Environment
    Resolve,
    runResolve,

    -- * Global context
    Context(..),
    baseContext, context, emptyContext,

    -- * Local context
    Locals,

    -- * Type resolving
    resolveProgram, resolveDecl, resolveStmt, resolveExpr, resolveValue, extractType,
) where

import Control.Monad.Reader
import Control.Monad.State

import              Data.List   (nub)
import              Data.Map    (Map)
import qualified    Data.Map    as Map
import              Data.Maybe  (fromJust)

import              Language.Qux.Annotated.Parser (SourcePos)
import              Language.Qux.Annotated.Syntax (simp)
import qualified    Language.Qux.Annotated.Syntax as Ann
import              Language.Qux.Context
import              Language.Qux.Syntax


-- | A type that allows resolving types.
--   Requires a 'Context' for evaluation.
type Resolve = Reader Context

-- | Runs the given resolve with the context.
runResolve :: Resolve a -> Context -> a
runResolve = runReader


-- | Local context.
--   This is a map of variable names to types (e.g., parameters).
type Locals = Map Id Type


-- | Resolves the types of a program.
resolveProgram :: Ann.Program SourcePos -> Resolve (Ann.Program SourcePos)
resolveProgram (Ann.Program pos module_ decls) = mapM resolveDecl decls >>= \decls' -> return $ Ann.Program pos module_ decls'

-- | Resolves the types of a declaration.
resolveDecl :: Ann.Decl SourcePos -> Resolve (Ann.Decl SourcePos)
resolveDecl (Ann.FunctionDecl pos attrs name type_ stmts)   = do
    stmts' <- evalStateT (resolveBlock stmts) (Map.fromList [(simp p, simp t) | (t, p) <- type_])

    return $ Ann.FunctionDecl pos attrs name type_ stmts'
resolveDecl decl                                            = return decl

resolveBlock :: [Ann.Stmt SourcePos] -> StateT Locals Resolve [Ann.Stmt SourcePos]
resolveBlock = mapM resolveStmt

-- | Resolves the types of a statement.
resolveStmt :: Ann.Stmt SourcePos -> StateT Locals Resolve (Ann.Stmt SourcePos)
resolveStmt (Ann.IfStmt pos condition trueStmts falseStmts) = do
    condition'  <- resolveExpr condition
    trueStmts'  <- resolveBlock trueStmts
    falseStmts' <- resolveBlock falseStmts

    return $ Ann.IfStmt pos condition' trueStmts' falseStmts'
resolveStmt (Ann.ReturnStmt pos expr)                       = do
    expr' <- resolveExpr expr

    return $ Ann.ReturnStmt pos expr'
resolveStmt (Ann.WhileStmt pos condition stmts)             = do
    condition'  <- resolveExpr condition
    stmts'      <- resolveBlock stmts

    return $ Ann.WhileStmt pos condition' stmts'

-- | Resolves the types of an expression.
resolveExpr :: Ann.Expr SourcePos -> StateT Locals Resolve (Ann.Expr SourcePos)
resolveExpr (Ann.ApplicationExpr {})        = error "internal error: cannot resolve the type of a non-qualified expression (try applying name resolution)"
resolveExpr (Ann.BinaryExpr pos op lhs rhs) = do
    lhs' <- resolveExpr lhs
    rhs' <- resolveExpr rhs

    let type_ = case op of
            Acc -> let (ListType inner) = extractType lhs' in inner
            Mul -> IntType
            Div -> IntType
            Mod -> IntType
            Add -> extractType lhs'
            Sub -> extractType lhs'
            Lt  -> BoolType
            Lte -> BoolType
            Gt  -> BoolType
            Gte -> BoolType
            Eq  -> BoolType
            Neq -> BoolType

    return $ Ann.TypedExpr pos type_ (Ann.BinaryExpr pos op lhs' rhs')
resolveExpr (Ann.CallExpr pos id arguments) = asks (Map.lookup (map simp id) . functions) >>= maybe
-- TODO (hjw): raise an exception rather than fail
    (error "internal error: undefined function call has no type")
    (\type_ -> do
        arguments_ <- mapM resolveExpr arguments

        return $ Ann.TypedExpr pos (fst $ last type_) (Ann.CallExpr pos id arguments_))
resolveExpr (Ann.ListExpr pos elements)     = do
    elements' <- mapM resolveExpr elements

    let types = map extractType elements'

    case length (nub types) == 1 of
        True    -> return $ Ann.TypedExpr pos (ListType $ head types) (Ann.ListExpr pos elements')
        False   -> error "internal error: top type not implemented"
resolveExpr e@(Ann.TypedExpr {})            = return e
resolveExpr (Ann.UnaryExpr pos op expr)     = do
    expr' <- resolveExpr expr

    return $ Ann.TypedExpr pos IntType (Ann.UnaryExpr pos op expr')
resolveExpr e@(Ann.ValueExpr pos value)     = return $ Ann.TypedExpr pos (resolveValue value) e
resolveExpr e@(Ann.VariableExpr pos name)   = gets (fromJust . Map.lookup (simp name)) >>= \type_ -> return $ Ann.TypedExpr pos type_ e

-- | Resolves the type of a value.
resolveValue :: Value -> Type
resolveValue (BoolValue _)          = BoolType
resolveValue (IntValue _)           = IntType
resolveValue (ListValue elements)   = case length (nub types) == 1 of
    True    -> ListType $ head types
    False   -> error "internal error: top type not implemented"
    where
        types = map resolveValue elements
resolveValue NilValue               = NilType

-- | Extracts the type from a 'Ann.TypedExpr'.
--   If the expression isn't an 'Ann.TypedExpr', an error is raised.
extractType :: Ann.Expr a -> Type
extractType (Ann.TypedExpr _ type_ _)  = type_
extractType _                          = error "internal error: type resolution not complete"

