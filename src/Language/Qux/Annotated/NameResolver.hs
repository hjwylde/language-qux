{-|
Module      : Language.Qux.Annotated.NameResolver
Description : Name resolving functions that transform the abstract syntax tree to a name-resolved one.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

Name resolving functions that transform the abstract syntax tree to a name-resolved one.

These functions will transform every 'Ann.ApplicationExpr' into either a 'Ann.CallExpr' or
    'Ann.VariableExpr' and return the transformed tree.
The "Language.Qux.Annotated.TypeChecker" and "Language.Qux.Llvm.Compiler" modules require the tree
    to have all names fully resolved.
-}

module Language.Qux.Annotated.NameResolver (
    module Language.Qux.Context,

    -- * Environment
    Resolve,
    runResolve,

    -- * Local context
    Locals,

    -- * Name resolving
    resolveProgram, resolveDecl, resolveStmt, resolveExpr,
) where

import Control.Lens         hiding (Context)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import           Data.Function
import           Data.List.Extra
import qualified Data.Map        as Map

import           Language.Qux.Annotated.Exception
import           Language.Qux.Annotated.Parser
import           Language.Qux.Annotated.Syntax    (simp)
import qualified Language.Qux.Annotated.Syntax    as Ann
import           Language.Qux.Context
import           Language.Qux.Syntax

-- | A type that allows resolving types.
--   Requires a 'Context' for evaluation.
type Resolve = ReaderT Context (Writer [ResolveException])

-- | Runs the given resolve with the context.
runResolve :: Resolve a -> Context -> (a, [ResolveException])
runResolve resolve context = runWriter $ runReaderT resolve context

functionFromName :: Ann.Id SourcePos -> Resolve [Id]
functionFromName (Ann.Id pos name) = do
    ids <- views (withName name . functions) Map.keys

    when (null ids)         $ tell [UndefinedFunctionCall pos name]
    when (length ids > 1)   $ tell [AmbiguousFunctionCall pos name (map init ids)]

    return $ head ids

-- | Local context.
--   This is a list of local variable names.
type Locals = [Id]

-- | Resolves the names of a program.
resolveProgram :: Ann.Program SourcePos -> Resolve (Ann.Program SourcePos)
resolveProgram (Ann.Program pos module_ decls) = do
    let imports     =  [import_ | import_@(Ann.ImportDecl {}) <- decls]
    foundImports    <- views (imported . functions) (nub . map init . Map.keys)

    let unfoundImports = filter (\(Ann.ImportDecl _ id) -> map simp id `notElem` foundImports) imports
    unless (null unfoundImports) $ tell [ImportNotFound pos (map simp id) | (Ann.ImportDecl pos id) <- unfoundImports]

    let duplicateImports = concat $ filter ((> 1) . length) (groupBy ((==) `on` simp) imports)
    unless (null duplicateImports) $ tell [DuplicateImport pos (map simp id) | (Ann.ImportDecl pos id) <- duplicateImports]

    mapM resolveDecl decls >>= \decls' -> return $ Ann.Program pos module_ decls'

-- | Resolves the names of a declaration.
resolveDecl :: Ann.Decl SourcePos -> Resolve (Ann.Decl SourcePos)
resolveDecl (Ann.FunctionDecl pos attrs name type_ stmts)   = do
    let duplicateAttrs = concat $ filter ((> 1) . length) (groupBy ((==) `on` simp) attrs)
    unless (null duplicateAttrs) $ tell [DuplicateAttribute (Ann.ann attr) (lower . show $ simp attr) | attr <- duplicateAttrs]

    stmts' <- evalStateT (resolveBlock stmts) (map (simp . snd) type_)

    return $ Ann.FunctionDecl pos attrs name type_ stmts'
resolveDecl decl@(Ann.TypeDecl _ attrs _)                   = do
    let duplicateAttrs = concat $ filter ((> 1) . length) (groupBy ((==) `on` simp) attrs)
    unless (null duplicateAttrs) $ tell [DuplicateAttribute (Ann.ann attr) (lower . show $ simp attr) | attr <- duplicateAttrs]

    return decl
resolveDecl decl                                            = return decl

resolveBlock :: [Ann.Stmt SourcePos] -> StateT Locals Resolve [Ann.Stmt SourcePos]
resolveBlock = mapM resolveStmt

-- | Resolves the names of a statement.
resolveStmt :: Ann.Stmt SourcePos -> StateT Locals Resolve (Ann.Stmt SourcePos)
resolveStmt (Ann.IfStmt pos condition trueStmts falseStmts) = do
    condition'  <- resolveExpr condition
    trueStmts'  <- resolveBlock trueStmts
    falseStmts' <- resolveBlock falseStmts

    return $ Ann.IfStmt pos condition' trueStmts' falseStmts'
resolveStmt (Ann.CallStmt pos expr)                         = do
    expr' <- resolveExpr expr

    return $ Ann.CallStmt pos expr'
resolveStmt (Ann.ReturnStmt pos mExpr)                      = do
    mExpr' <- mapM resolveExpr mExpr

    return $ Ann.ReturnStmt pos mExpr'
resolveStmt (Ann.WhileStmt pos condition stmts)             = do
    condition'  <- resolveExpr condition
    stmts'      <- resolveBlock stmts

    return $ Ann.WhileStmt pos condition' stmts'

-- | Resolves the names of an expression.
resolveExpr :: Ann.Expr SourcePos -> StateT Locals Resolve (Ann.Expr SourcePos)
resolveExpr (Ann.ApplicationExpr pos name arguments)    = gets (elem $ simp name) >>= \member -> if member
    then do
        unless (null arguments) $ tell [InvalidVariableAccess pos (simp name)]

        return $ Ann.VariableExpr pos name
    else do
        id          <- lift $ functionFromName name
        arguments_  <- mapM resolveExpr arguments

        return $ Ann.CallExpr pos (map (Ann.Id $ Ann.ann name) id) arguments_
resolveExpr (Ann.BinaryExpr pos op lhs rhs)             = do
    lhs' <- resolveExpr lhs
    rhs' <- resolveExpr rhs

    return $ Ann.BinaryExpr pos op lhs' rhs'
resolveExpr e@(Ann.CallExpr {})                         = return e
resolveExpr (Ann.TypedExpr pos type_ expr)              = do
    expr' <- resolveExpr expr

    return $ Ann.TypedExpr pos type_ expr'
resolveExpr (Ann.UnaryExpr pos op expr)                 = do
    expr' <- resolveExpr expr

    return $ Ann.UnaryExpr pos op expr'
resolveExpr e@(Ann.ValueExpr {})                        = return e
resolveExpr e@(Ann.VariableExpr {})                     = return e
