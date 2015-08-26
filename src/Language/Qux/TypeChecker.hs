
{-|
Module      : Language.Qux.TypeChecker
Description : Type checking functions to verify that a 'Program' is type-safe.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Type checking functions to verify that a 'Program' is type-safe.

These functions only verify that types are used correctly.
They don't verify other properties such as definite assignment.
-}

module Language.Qux.TypeChecker (
    -- * Environment
    Env,
    buildEnv,

    -- * Type checking
    Check,

    -- ** Program checking
    check,

    -- ** Other node checking
    checkProgram, checkDecl, checkStmt, checkExpr, checkValue
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State

import Data.Function
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple

import Language.Qux.Exception
import Language.Qux.Syntax


-- |    An environment that holds the current state of identifiers to types.
--      An identifier is a function name and the types are it's arguments and return type (in
--      order).
type Env = State (Map Id [Type])

-- | Builds an initial environment from a program's declarations.
buildEnv :: Program -> Map Id [Type]
buildEnv (Program decls) = foldl Map.union Map.empty (map buildEnv' decls)

buildEnv' :: Decl -> Map Id [Type]
buildEnv' (FunctionDecl name parameters _) = Map.singleton name (map fst parameters)


-- |    Either a 'TypeException' or an @a@.
--      Contains an underlying 'Env' in the monad transformer.
type Check = ExceptT TypeException Env


-- |    Type checks the program.
--      If an exception occurs then the result will be a 'TypeException', otherwise 'Nothing'.
--      This function wraps 'checkProgram' by building and evaluating the 'Env' under the hood.
check :: Program -> Maybe TypeException
check program = either Just (const Nothing) $
    evalState (runExceptT $ checkProgram program) (buildEnv program)


-- | Type checks a program.
checkProgram :: Program -> Check ()
checkProgram (Program decls)
    | null duplicateNames   = mapM_ checkDecl decls
    | otherwise             = throwError $ duplicateFunctionName (head duplicateNames)
    where
        functionNames   = map (\(FunctionDecl name _ _) -> name) decls
        duplicateNames  = functionNames \\ nub functionNames

-- | Type checks a declaration.
checkDecl :: Decl -> Check ()
checkDecl (FunctionDecl _ parameters stmts)
    | null duplicateNames   = ExceptT $ runStateWith (runExceptT $ checkBlock stmts) $ Map.union (Map.fromList $ map (mapSnd (:[])) (map swap parameters))
    | otherwise             = throwError $ duplicateParameterName (head duplicateNames)
    where
        paramNames      = map snd parameters
        duplicateNames  = paramNames \\ nub paramNames
        mapSnd f (a, b) = (a, f b)

checkBlock :: [Stmt] -> Check ()
checkBlock = mapM_ checkStmt

-- | Type checks a statement.
checkStmt :: Stmt -> Check ()
checkStmt (IfStmt condition trueStmts falseStmts)   = do
    expectExpr condition [BoolType]

    checkBlock trueStmts
    checkBlock falseStmts
checkStmt (ReturnStmt expr)                         = do
    env <- get

    void $ expectExpr expr (env ! "@")
checkStmt (WhileStmt condition stmts)               = do
    expectExpr condition [BoolType]

    checkBlock stmts

-- | Type checks an expression.
checkExpr :: Expr -> Check Type
checkExpr (ApplicationExpr name arguments)  = do
    env <- get
    let expected = init $ env ! name

    case length expected == length arguments of
        True -> zipWithM expectExpr arguments $ map (:[]) expected
        False -> throwError $ invalidArgumentsCount (length expected) (length arguments)

    return $ head (env ! "@")
checkExpr (BinaryExpr op lhs rhs)
    | op `elem` [Acc]               = expectExpr lhs [ListType undefined] <* expectExpr rhs [IntType]
    | op `elem` [Mul, Div, Mod]     = expectExpr lhs [IntType] >> expectExpr rhs [IntType]
    | op `elem` [Add, Sub]          = checkExpr lhs >>= \lhs' -> expectExpr rhs [lhs'] >>= expectType [IntType, ListType undefined]
    | op `elem` [Lt, Lte, Gt, Gte]  = expectExpr lhs [IntType] >> expectExpr rhs [IntType] *> return BoolType
    | op `elem` [Eq, Neq]           = ((:[]) <$> checkExpr lhs >>= expectExpr rhs) *> return BoolType
checkExpr (ListExpr elements)               = mapM checkExpr elements >>= checkElementTypes
checkExpr (UnaryExpr op expr)
    | op `elem` [Len]               = expectExpr expr [ListType undefined] *> return IntType
    | op `elem` [Neg]               = expectExpr expr [IntType]
checkExpr (ValueExpr value)                 = checkValue value

-- | Type checks a value.
checkValue :: Value -> Check Type
checkValue (BoolValue _)        = return BoolType
checkValue (IntValue _)         = return IntType
checkValue (ListValue elements) = mapM checkValue elements >>= checkElementTypes
checkValue NilValue             = return NilType


expectType :: [Type] -> Type -> Check Type
expectType expects received
    | received `elem` expects   = return received
    | otherwise                 = throwError $ mismatchedType expects received

expectExpr :: Expr -> [Type] -> Check Type
expectExpr expr expects = checkExpr expr >>= expectType expects

checkElementTypes :: [Type] -> Check Type
checkElementTypes [] = undefined
checkElementTypes xs
    | length (nub xs) == 1  = return $ head xs
    | otherwise             = throwError $ mismatchedType (take 1 xs) (nub xs !! 1)


runStateWith :: State s a -> (s -> s) -> State s a
runStateWith r f = state $ \s -> (evalState r (f s), s)

