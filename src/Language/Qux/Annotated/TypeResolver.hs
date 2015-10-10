
{-|
Module      : Language.Qux.Annotated.TypeResolver
Description : Type resolving functions that transform the abstract syntax tree to a typed one.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Type resolving functions that transform the abstract syntax tree to a typed one.

These functions will transform every 'Ann.Expr' into an 'Ann.TypedExpr' and return the transformed
tree.
The "Language.Qux.Annotated.TypeChecker" module requires the tree to be typed.
-}

module Language.Qux.Annotated.TypeResolver (
    -- * Environment
    Resolve,
    runResolve,

    -- * Global context
    Context(..),
    context, emptyContext,

    -- * Local context
    Locals,

    -- * Type resolving
    resolve, resolveProgram, resolveDecl, resolveStmt, resolveExpr, resolveValue,
    extractType
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import              Data.List   (nub)
import              Data.Map    (Map)
import qualified    Data.Map    as Map

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


-- | Global context that holds function definition types.
data Context = Context {
    functions :: Map Id [Type] -- ^ A map of function names to parameter types.
    }

-- | Returns a context for the given program.
context :: Program -> Context
context (Program _ decls) = Context {
    functions = Map.fromList $ [(name, map fst parameters) | (FunctionDecl name parameters _) <- decls]
    }

-- | An empty context.
emptyContext :: Context
emptyContext = Context { functions = Map.empty }


-- | Local context.
--   This is a map of variable names to types (e.g., parameters).
type Locals = Map Id Type

retrieve :: Id -> StateT Locals Resolve (Maybe [Type])
retrieve name = do
    maybeLocal  <- gets $ (fmap (:[])) . (Map.lookup name)
    maybeDef    <- asks $ (Map.lookup name) . functions

    return $ maybeLocal <|> maybeDef


-- | Resolves the types of the program, returning the modified syntax tree.
resolve :: Ann.Program SourcePos -> Ann.Program SourcePos
resolve program = runResolve (resolveProgram program) (context $ simp program)

-- | Resolves the types of a program.
resolveProgram :: Ann.Program SourcePos -> Resolve (Ann.Program SourcePos)
resolveProgram (Ann.Program pos module_ decls) = mapM resolveDecl decls >>= \decls' -> return $ Ann.Program pos module_ decls'

-- | Resolves the types of a declaration.
resolveDecl :: Ann.Decl SourcePos -> Resolve (Ann.Decl SourcePos)
resolveDecl (Ann.FunctionDecl pos name parameters stmts) = do
    stmts' <- evalStateT (resolveBlock stmts) (Map.fromList [(simp p, simp t) | (t, p) <- parameters])

    return $ Ann.FunctionDecl pos name parameters stmts'

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
resolveExpr (Ann.ApplicationExpr pos name arguments)    = retrieve (simp name) >>= maybe
    (error "internal error: undefined function call has no type (try applying name resolution)")
    (\types -> do
        arguments_ <- mapM resolveExpr arguments

        return $ Ann.TypedExpr pos (last types) (Ann.ApplicationExpr pos name arguments_))
resolveExpr (Ann.BinaryExpr pos op lhs rhs)             = do
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
resolveExpr (Ann.ListExpr pos elements)                 = do
    elements' <- mapM resolveExpr elements

    let types = map extractType elements'

    case length (nub types) == 1 of
        True    -> return $ Ann.TypedExpr pos (ListType $ head types) (Ann.ListExpr pos elements')
        False   -> error "internal error: top type not implemented"
resolveExpr e@(Ann.TypedExpr _ _ _)                     = return e
resolveExpr (Ann.UnaryExpr pos op expr)                 = do
    expr' <- resolveExpr expr

    return $ Ann.TypedExpr pos IntType (Ann.UnaryExpr pos op expr')
resolveExpr e@(Ann.ValueExpr pos value)                 = return $ Ann.TypedExpr pos (resolveValue value) e

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

