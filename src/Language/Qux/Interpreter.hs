
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
    -- * Environments
    Execution,
    runExecution,
    Evaluation,
    runEvaluation,

    -- * Global context
    Context,
    context, emptyContext,

    -- * Local context
    Locals,

    -- * Interpreter

    -- ** Program  execution
    exec, execBlock, execStmt,

    -- ** Expression evaluation
    evalExpr
) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Either

import Data.List ((\\))
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

import Language.Qux.Syntax


-- |    An environment that holds the global state (@Reader Context@) and the local state
--      (@Locals@).
--      It supports breaking out of execution (via 'EitherT Value').
type Execution = EitherT Value Evaluation

-- |    Runs the execution, returning the left result if present otherwise converting @a@ with the
--      given function.
runExecution :: Execution a -> (a -> Value) -> Evaluation Value
runExecution exec f = either id f <$> runEitherT exec

-- |    An environment that holds the global state (@Reader Context@) and the local state
--      (@Locals@).
--      Purely for evaluation of expressions---this environment does not support breaking out of
--      execution.
type Evaluation = StateT Locals (Reader Context)

runEvaluation :: Evaluation a -> Context -> Locals -> a
runEvaluation eval context locals = runReader (evalStateT eval locals) context


-- |    Global context that holds function definitions.
--      The function name, parameter names and statements are held.
data Context = Context {
    functions :: Map Id ([Id], [Stmt])
    }

-- | Returns a context for the given program.
context :: Program -> Context
context (Program decls) = Context { functions = Map.fromList $ map (\d -> (name d, (parameterNames d, stmts d))) decls }

-- | An empty context.
emptyContext :: Context
emptyContext = Context { functions = Map.empty }

-- | Local context.
type Locals = Map Id Value


-- |    @exec program entry arguments@ executes @entry@ (passing it @arguments@) in the context
--      of @program@.
--      This function wraps 'execFunction' by building and evaluating the environment under
--      the hood.
exec :: Program -> Id -> [Value] -> Value
exec program entry arguments = runEvaluation (evalApplicationExpr entry arguments) (context program) Map.empty

execBlock :: [Stmt] -> Execution ()
execBlock = mapM_ execStmt

-- | Executes the statement in a breaking environment.
execStmt :: Stmt -> Execution ()
execStmt (IfStmt condition trueStmts falseStmts) = do
    result <- lift $ evalExpr condition

    execBlock $ if runBoolValue result then trueStmts else falseStmts
execStmt (ReturnStmt expr)                       = lift (evalExpr expr) >>= left
execStmt s@(WhileStmt condition stmts)           = do
    result <- lift $ evalExpr condition

    when (runBoolValue result) $ execBlock stmts >> execStmt s

-- | Reduces the expression to a value (normal form).
evalExpr :: Expr -> Evaluation Value
evalExpr (ApplicationExpr name arguments) = mapM evalExpr arguments >>= evalApplicationExpr name
evalExpr (BinaryExpr op lhs rhs)          = do
    lhs' <- evalExpr lhs
    rhs' <- evalExpr rhs

    evalBinaryExpr op lhs' rhs'
evalExpr (ListExpr exprs)                 = ListValue <$> mapM evalExpr exprs
evalExpr (UnaryExpr op expr)              = evalExpr expr >>= evalUnaryExpr op
evalExpr (ValueExpr value)                = return value

evalApplicationExpr :: Id -> [Value] -> Evaluation Value
evalApplicationExpr name arguments = do
    maybeValue <- gets $ Map.lookup name

    if isJust maybeValue then
        when (not $ null arguments) (error "malformed program") >> return $ fromJust maybeValue
    else do
        (parameters, stmts) <- asks $ (! name) . functions

        lift $ evalStateT (runExecution (execBlock stmts) undefined) (Map.fromList $ zip parameters arguments)

evalBinaryExpr :: BinaryOp -> Value -> Value -> Evaluation Value
evalBinaryExpr Acc (ListValue elements) (IntValue rhs)  = return $ elements !! (fromInteger rhs)
evalBinaryExpr Mul (IntValue lhs)  (IntValue rhs)       = return $ IntValue   (lhs * rhs)
evalBinaryExpr Div (IntValue lhs)  (IntValue rhs)       = return $ IntValue   (lhs `div` rhs)
evalBinaryExpr Mod (IntValue lhs)  (IntValue rhs)       = return $ IntValue   (lhs `mod` rhs)
evalBinaryExpr Add (IntValue lhs)  (IntValue rhs)       = return $ IntValue   (lhs + rhs)
evalBinaryExpr Add (ListValue lhs) (ListValue rhs)      = return $ ListValue  (lhs ++ rhs)
evalBinaryExpr Sub (IntValue lhs)  (IntValue rhs)       = return $ IntValue   (lhs - rhs)
evalBinaryExpr Sub (ListValue lhs) (ListValue rhs)      = return $ ListValue  (lhs \\ rhs)
evalBinaryExpr Lt  (IntValue lhs)  (IntValue rhs)       = return $ BoolValue  (lhs < rhs)
evalBinaryExpr Lte (IntValue lhs)  (IntValue rhs)       = return $ BoolValue  (lhs <= rhs)
evalBinaryExpr Gt  (IntValue lhs)  (IntValue rhs)       = return $ BoolValue  (lhs > rhs)
evalBinaryExpr Gte (IntValue lhs)  (IntValue rhs)       = return $ BoolValue  (lhs >= rhs)
evalBinaryExpr Eq  (BoolValue lhs) (BoolValue rhs)      = return $ BoolValue  (lhs == rhs)
evalBinaryExpr Eq  (IntValue lhs)  (IntValue rhs)       = return $ BoolValue  (lhs == rhs)
evalBinaryExpr Eq  (ListValue lhs) (ListValue rhs)      = return $ BoolValue  (lhs == rhs)
evalBinaryExpr Eq  NilValue        NilValue             = return $ BoolValue  True
evalBinaryExpr Eq  _               _                    = return $ BoolValue  False
evalBinaryExpr Neq lhs             rhs                  = evalBinaryExpr Eq lhs rhs >>= return . BoolValue . not . runBoolValue
evalBinaryExpr _ _ _                                    = error "malformed program"

evalUnaryExpr :: UnaryOp -> Value -> Evaluation Value
evalUnaryExpr Len (ListValue elements)  = return $ IntValue (toInteger $ length elements)
evalUnaryExpr Neg (IntValue value)      = return $ IntValue (-value)
evalUnaryExpr _ _                       = error "malformed program"


runBoolValue :: Value -> Bool
runBoolValue (BoolValue value)  = value
runBoolValue _                  = error "malformed program"

