
{-|
Module      : Language.Qux.Annotated.NameResolver
Description : Name resolving functions that transform the abstract syntax tree to a name-resolved one.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Name resolving functions that transform the abstract syntax tree to a name-resolved one.

These functions will transform every 'Ann.ApplicationExpr' into either a 'Ann.CallExpr' or
    'Ann.VariableExpr' and return the transformed tree.
The "Language.Qux.Annotated.TypeChecker" and "Language.Qux.Llvm.Compiler" modules require the tree
    to have all names fully resolved.
-}

module Language.Qux.Annotated.NameResolver (
    -- * Environment
    Resolve,
    runResolve,

    -- * Global context
    Context(..),
    context,

    -- * Local context
    Locals,

    -- * Name resolving
    resolveProgram, resolveDecl, resolveStmt, resolveExpr,
) where

import Control.Monad.Reader
import Control.Monad.State

import              Data.Set (Set, member)
import qualified    Data.Set as Set

import              Language.Qux.Annotated.Parser (SourcePos)
import              Language.Qux.Annotated.Syntax (simp)
import qualified    Language.Qux.Annotated.Syntax as Ann
import              Language.Qux.Syntax


-- | A type that allows resolving types.
--   Requires a 'Context' for evaluation.
type Resolve = Reader Context

-- | Runs the given resolve with the context.
runResolve :: Resolve a -> Context -> a
runResolve = runReader


-- | Global context that holds the current module identifier.
data Context = Context {
    module_     :: [Id],    -- ^ The current module identifier.
    functions   :: [[Id]]   -- ^ A list of available qualified function identifiers.
                            --   This includes the current module's functions and imported
                            --   functions.
    }
    deriving (Eq, Show)

-- | Returns a context for the given program.
--   Requires the main program being resolved and the contextual programs (to resolve imports from).
context :: Program -> [Program] -> Context
context program@(Program m decls) programs = Context {
    module_     = m,
    functions   = [m' ++ [name] | (Program m' decls') <- program:programs, m' `elem` imports, (FunctionDecl _ name _ _) <- decls']
    }
    where
        imports = m:[id | (ImportDecl id) <- decls]

functionsFromName :: Id -> Context -> [[Id]]
functionsFromName name = (filter $ (==) name . last) . functions


-- | Local context.
--   This is a set of local variable names.
type Locals = Set Id


-- | Resolves the names of a program.
resolveProgram :: Ann.Program SourcePos -> Resolve (Ann.Program SourcePos)
resolveProgram (Ann.Program pos module_ decls) = mapM resolveDecl decls >>= \decls' -> return $ Ann.Program pos module_ decls'

-- | Resolves the names of a declaration.
resolveDecl :: Ann.Decl SourcePos -> Resolve (Ann.Decl SourcePos)
resolveDecl (Ann.FunctionDecl pos attrs name type_ stmts)   = do
    stmts' <- evalStateT (resolveBlock stmts) (Set.fromList $ map (simp . snd) type_)

    return $ Ann.FunctionDecl pos attrs name type_ stmts'
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
resolveStmt (Ann.ReturnStmt pos expr)                       = do
    expr' <- resolveExpr expr

    return $ Ann.ReturnStmt pos expr'
resolveStmt (Ann.WhileStmt pos condition stmts)             = do
    condition'  <- resolveExpr condition
    stmts'      <- resolveBlock stmts

    return $ Ann.WhileStmt pos condition' stmts'

-- | Resolves the names of an expression.
resolveExpr :: Ann.Expr SourcePos -> StateT Locals Resolve (Ann.Expr SourcePos)
resolveExpr (Ann.ApplicationExpr pos name arguments)    = gets (member $ simp name) >>= \member -> case member of
    True    -> do
        -- TODO (hjw): what if an argument was passed on a local variable access?
        when (length arguments /= 0) $ error "internal error: arguments passed to a local variable access"

        return $ Ann.VariableExpr pos name
    False   -> do
        ids         <- asks $ functionsFromName (simp name)
        arguments_  <- mapM resolveExpr arguments

        -- TODO (hjw): what if there is more than one exporter of the function?
        when (length ids /= 1) $ error ("internal error: function \"" ++ simp name ++ "\" is exported by multiple modules")

        return $ Ann.CallExpr pos (map (Ann.Id $ Ann.ann name) (head ids)) arguments_
resolveExpr (Ann.BinaryExpr pos op lhs rhs)             = do
    lhs' <- resolveExpr lhs
    rhs' <- resolveExpr rhs

    return $ Ann.BinaryExpr pos op lhs' rhs'
resolveExpr e@(Ann.CallExpr {})                         = return e
resolveExpr (Ann.ListExpr pos elements)                 = do
    elements' <- mapM resolveExpr elements

    return $ Ann.ListExpr pos elements'
resolveExpr (Ann.TypedExpr pos type_ expr)              = do
    expr' <- resolveExpr expr

    return $ Ann.TypedExpr pos type_ expr'
resolveExpr (Ann.UnaryExpr pos op expr)                 = do
    expr' <- resolveExpr expr

    return $ Ann.UnaryExpr pos op expr'
resolveExpr e@(Ann.ValueExpr {})                        = return e
resolveExpr e@(Ann.VariableExpr {})                     = return e

