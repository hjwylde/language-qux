
{-|
Module      : Language.Qux.Annotated.TypeChecker
Description : Type checking functions to verify that a 'Program' is type-safe.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Type checking functions to verify that a 'Program' is type-safe.

These functions only verify that types are used correctly.
They don't verify other properties such as definite assignment.
-}

module Language.Qux.Annotated.TypeChecker (
    module Language.Qux.Context,

    -- * Environment
    Check,
    runCheck, execCheck,

    -- * Local context
    Locals,

    -- * Type checking
    checkProgram, checkDecl, checkStmt, checkExpr,
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import              Data.Function   (on)
import              Data.List       (groupBy, sortOn)
import qualified    Data.Map        as Map

import              Language.Qux.Annotated.Exception
import              Language.Qux.Annotated.Parser       (SourcePos)
import              Language.Qux.Annotated.Syntax       (simp)
import qualified    Language.Qux.Annotated.Syntax       as Ann
import              Language.Qux.Annotated.TypeResolver
import              Language.Qux.Context
import              Language.Qux.Syntax


-- | A type that allows collecting errors while type checking a program.
--   Requires a 'Context' for evaluation.
type Check = ReaderT Context (Writer [TypeException])

-- | Runs the given check with the context.
runCheck :: Check a -> Context -> (a, [TypeException])
runCheck check context = runWriter $ runReaderT check context

-- | Runs the given check with the context and extracts the exceptions.
execCheck :: Check a -> Context -> [TypeException]
execCheck check context = execWriter $ runReaderT check context


-- | Type checks a program.
checkProgram :: Ann.Program SourcePos -> Check ()
checkProgram (Ann.Program _ _ decls)
    | null duplicates   = mapM_ checkDecl decls
    | otherwise         = tell $ map (uncurry DuplicateFunctionName) duplicates
    where
        duplicates      = concat $ filter ((> 1) . length) (groupBy ((==) `on` snd) functionDecls)
        functionDecls   = sortOn snd [(pos, name) | (Ann.FunctionDecl _ _ (Ann.Id pos name) _ _) <- decls]

-- | Type checks a declaration.
checkDecl :: Ann.Decl SourcePos -> Check ()
checkDecl (Ann.FunctionDecl _ _ _ type_ stmts)
    | null duplicates   = evalStateT (checkBlock stmts) (Map.fromList [(simp p, simp t) | (t, p) <- type_])
    | otherwise         = tell $ map (uncurry DuplicateParameterName) duplicates
    where
        duplicates = concat $ filter ((> 1) . length) (groupBy ((==) `on` snd) parameterNames)
        parameterNames = sortOn snd [(pos, name) | (_, Ann.Id pos name) <- type_]
checkDecl _                                         = return ()

checkBlock :: [Ann.Stmt SourcePos] -> StateT Locals Check ()
checkBlock = mapM_ checkStmt

-- | Type checks a statement.
checkStmt :: Ann.Stmt SourcePos -> StateT Locals Check ()
checkStmt (Ann.IfStmt _ condition trueStmts falseStmts)   = do
    expectExpr_ condition [BoolType]

    checkBlock trueStmts
    checkBlock falseStmts
checkStmt (Ann.ReturnStmt _ expr)                         = do
    expected <- gets (Map.! "@")

    expectExpr_ expr [expected]
checkStmt (Ann.WhileStmt _ condition stmts)               = do
    expectExpr_ condition [BoolType]

    checkBlock stmts

-- | Type checks an expression.
checkExpr :: Ann.Expr SourcePos -> StateT Locals Check Type
checkExpr (Ann.TypedExpr _ type_ (Ann.BinaryExpr _ op lhs rhs))
    | op `elem` [Acc]                       = expectExpr_ lhs [ListType type_] >> expectExpr_ rhs [IntType] >> return type_
    | op `elem` [Mul, Div, Mod, Add, Sub]   = expectExpr_ lhs [type_] >> expectExpr rhs [type_]
    | op `elem` [Lt, Lte, Gt, Gte]          = expectExpr_ lhs [IntType] >> expectExpr_ rhs [IntType] >> return type_
    | op `elem` [Eq, Neq]                   = checkExpr lhs >>= expectExpr rhs . (:[]) >> return type_
    | otherwise                             = error $ "internal error: type checking for \"" ++ show op ++ "\" not implemented"
checkExpr (Ann.TypedExpr _ type_ (Ann.CallExpr pos id arguments))   = asks (Map.lookup (map simp id) . functions) >>= maybe
    (error "internal error: undefined function call has no type (try applying name resolution)")
    (\types -> do
        let expected = map fst (init types)

        zipWithM_ expectExpr arguments $ map (:[]) expected

        when (length expected /= length arguments) $ tell [InvalidFunctionCall pos (length arguments) (length expected)]

        return type_)
checkExpr (Ann.TypedExpr _ type_ (Ann.ListExpr _ elements))         = do
    let (ListType inner) = type_

    mapM_ (`expectExpr` [inner]) elements

    return type_
checkExpr (Ann.TypedExpr _ type_ (Ann.UnaryExpr _ op expr))
    | op `elem` [Len]   = expectExpr_ expr [ListType $ error "internal error: top type not implemented"] >> return type_
    | op `elem` [Neg]   = expectExpr expr [type_]
    | otherwise         = error $ "internal error: " ++ show op ++ " not implemented"
checkExpr (Ann.TypedExpr _ type_ (Ann.ValueExpr {}))                = return type_
checkExpr (Ann.TypedExpr _ type_ (Ann.VariableExpr {}))             = return type_
checkExpr _                                                         = error "internal error: cannot check the type of a non-typed expression (try applying type resolution)"


expectExpr :: Ann.Expr SourcePos -> [Type] -> StateT Locals Check Type
expectExpr expr expects = do
    type_ <- attach (Ann.ann expr) <$> checkExpr expr

    lift $ expectType type_ expects

expectExpr_ :: Ann.Expr SourcePos -> [Type] -> StateT Locals Check ()
expectExpr_ = fmap void . expectExpr

expectType :: Ann.Type SourcePos -> [Type] -> Check Type
expectType received expects
    | simp received `elem` expects  = return $ simp received
    | otherwise                     = do
        tell [MismatchedType (Ann.ann received) (simp received) expects]

        return $ simp received

attach :: SourcePos -> Type -> Ann.Type SourcePos
attach pos BoolType         = Ann.BoolType pos
attach pos CharType         = Ann.CharType pos
attach pos IntType          = Ann.IntType  pos
attach pos (ListType inner) = Ann.ListType pos (attach undefined inner)
attach pos NilType          = Ann.NilType  pos

