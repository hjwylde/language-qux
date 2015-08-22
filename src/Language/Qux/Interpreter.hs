
{-|
Module      : Language.Qux.Interpreter
Description : Functions for executing a program and retrieving the return result.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Functions for executing a program and retrieving the return result.

This module assumes the program is well-formed.
That is, it must be well-typed (see "Language.Qux.TypeChecker").
-}

module Language.Qux.Interpreter (
    -- * Environment
    Env, BreakingEnv,
    buildEnv,

    -- * Execution

    -- ** Program execution
    execute,

    -- ** Other node execution
    executeFunction, executeStmt,

    -- ** Expression reducing
    reduceExpr
) where

import Control.Monad.State
import Control.Monad.Trans.Either

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Language.Qux.Ast
import Language.Qux.Util


-- |    An environment that holds the current state of identifiers to declarations.
--      An identifier is a function name.
type Env = State (Map Id Decl)

-- | Builds an initial environment from a program's declarations.
buildEnv :: Program -> Map Id Decl
buildEnv (Program decls) = Map.fromList $ map (\d@(FunctionDecl name _ _) -> (name, d)) decls

-- | An 'EitherT' that enables breaking execution (e.g., due to return statements).
type BreakingEnv = EitherT Value Env


-- |    @execute program entry arguments@ executes @entry@ (passing it @arguments@) in the context
--      of @program@.
--      This function wraps 'executeFunction' by building and evaluating the 'Env' under the
--      hood.
execute :: Program -> Id -> [Value] -> Value
execute program entry arguments = evalState (executeFunction entry arguments) (buildEnv program)

-- | Executes the function with the given arguments, returning the result.
executeFunction :: Id -> [Value] -> Env Value
executeFunction name arguments = do
    maybeDecl <- Map.lookup name <$> get
    let (FunctionDecl _ parameters stmts) = fromJust maybeDecl
    let parametersMap = Map.fromList $ map
            (\(p, a) -> (p, FunctionDecl p [] [ReturnStmt $ ValueExpr a]))
            (zip (delete "@" $ map snd parameters) arguments)

    either id undefined <$> (
        runStateWith (runEitherT $ executeBlock stmts) (Map.union parametersMap)
        )

executeBlock :: [Stmt] -> BreakingEnv ()
executeBlock = mapM_ executeStmt

-- | Executes the statement in a breaking environment.
executeStmt :: Stmt -> BreakingEnv ()
executeStmt (IfStmt condition trueStmts falseStmts) = do
    result <- lift $ reduceExpr condition

    executeBlock $ case bool result of
        True    -> trueStmts
        False   -> falseStmts
executeStmt (ReturnStmt expr)                       = lift (reduceExpr expr) >>= left
executeStmt s@(WhileStmt condition stmts)           = do
    result <- lift $ reduceExpr condition

    when (bool result) $ executeBlock stmts >> executeStmt s

-- | Reduces the expression to a value (normal form).
reduceExpr :: Expr -> Env Value
reduceExpr (ApplicationExpr name arguments) = mapM reduceExpr arguments >>= executeFunction name
reduceExpr (InfixExpr op lhs rhs)           = do
    lhs' <- reduceExpr lhs
    rhs' <- reduceExpr rhs

    reduceInfixExpr op lhs' rhs'
reduceExpr (ListExpr exprs)                 = ListValue <$> mapM reduceExpr exprs
reduceExpr (ValueExpr value)                = return value


reduceInfixExpr :: InfixOp -> Value -> Value -> Env Value
reduceInfixExpr Acc (ListValue elements) (IntValue rhs) = return $ elements !! (fromInteger rhs)
reduceInfixExpr Mul (IntValue lhs)  (IntValue rhs)      = return $ IntValue (lhs * rhs)
reduceInfixExpr Div (IntValue lhs)  (IntValue rhs)      = return $ IntValue (lhs `div` rhs)
reduceInfixExpr Mod (IntValue lhs)  (IntValue rhs)      = return $ IntValue (lhs `mod` rhs)
reduceInfixExpr Add (IntValue lhs)  (IntValue rhs)      = return $ IntValue (lhs + rhs)
reduceInfixExpr Add (ListValue lhs) (ListValue rhs)     = return $ ListValue (lhs ++ rhs)
reduceInfixExpr Sub (IntValue lhs)  (IntValue rhs)      = return $ IntValue (lhs - rhs)
reduceInfixExpr Sub (ListValue lhs) (ListValue rhs)     = return $ ListValue (lhs \\ rhs)
reduceInfixExpr Lt  (IntValue lhs) (IntValue rhs)       = return $ BoolValue (lhs < rhs)
reduceInfixExpr Lte (IntValue lhs) (IntValue rhs)       = return $ BoolValue (lhs <= rhs)
reduceInfixExpr Gt  (IntValue lhs) (IntValue rhs)       = return $ BoolValue (lhs > rhs)
reduceInfixExpr Gte (IntValue lhs) (IntValue rhs)       = return $ BoolValue (lhs >= rhs)
reduceInfixExpr Eq  (BoolValue lhs) (BoolValue rhs)     = return $ BoolValue (lhs == rhs)
reduceInfixExpr Eq  (IntValue lhs)  (IntValue rhs)      = return $ BoolValue (lhs == rhs)
reduceInfixExpr Eq  (ListValue lhs) (ListValue rhs)     = return $ BoolValue (lhs == rhs)
reduceInfixExpr Eq  NilValue        NilValue            = return $ BoolValue True
reduceInfixExpr Eq  _               _                   = return $ BoolValue False
reduceInfixExpr Neq lhs             rhs                 = reduceInfixExpr Eq lhs rhs >>= return . BoolValue . not . bool

bool :: Value -> Bool
bool (BoolValue value) = value

