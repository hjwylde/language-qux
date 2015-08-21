
module Language.Qux.Interpreter (
    Env,
    buildEnv,
    executeMain, executeFunction
) where

import Control.Monad.State
import Control.Monad.Trans.Either

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Language.Qux.Ast
import Language.Qux.Util

type Env = State (Map Id Decl)

buildEnv :: Program -> Map Id Decl
buildEnv (Program decls) = Map.fromList $ map (\d@(FunctionDecl name _ _) -> (name, d)) decls

executeMain :: Env Value
executeMain = executeFunction "main" []

executeFunction :: Id -> [Value] -> Env Value
executeFunction name arguments = do
    maybeDecl <- Map.lookup name <$> get
    let (FunctionDecl _ parameters stmts) = fromJust maybeDecl
    let parametersMap = Map.fromList $ map
            (\(p, a) -> (p, FunctionDecl p [] [ReturnStmt $ ValueExpr a]))
            (zip (delete "@" $ map snd parameters) arguments)

    either id undefined <$> (runStateWith (runEitherT $ executeBlock stmts) $ Map.union parametersMap)

type BreakingEnv = EitherT Value Env

executeBlock :: [Stmt] -> BreakingEnv ()
executeBlock = mapM_ executeStmt

executeStmt :: Stmt -> BreakingEnv ()
executeStmt (IfStmt condition trueStmts falseStmts) = do
    result <- lift $ reduceExpr condition

    executeBlock $ case bool result of
        True -> trueStmts
        False -> falseStmts
executeStmt (ReturnStmt expr)                       = lift (reduceExpr expr) >>= left
executeStmt s@(WhileStmt condition stmts)           = do
    result <- lift $ reduceExpr condition

    case bool result of
        True -> executeBlock stmts >> executeStmt s
        False -> return ()

reduceExpr :: Expr -> Env Value
reduceExpr (ApplicationExpr name arguments) = mapM reduceExpr arguments >>= executeFunction name
reduceExpr (InfixExpr op lhs rhs)           = do
    lhs' <- reduceExpr lhs
    rhs' <- reduceExpr rhs

    reduceInfixExpr op lhs' rhs'
reduceExpr (ListExpr exprs)                 = ListValue <$> mapM reduceExpr exprs
reduceExpr (ValueExpr value)                = return value

reduceInfixExpr :: InfixOp -> Value -> Value -> Env Value
reduceInfixExpr Add (IntValue lhs)  (IntValue rhs)  = return $ IntValue (lhs + rhs)
reduceInfixExpr Add (ListValue lhs) (ListValue rhs) = return $ ListValue (lhs ++ rhs)
reduceInfixExpr Sub (IntValue lhs)  (IntValue rhs)  = return $ IntValue (lhs - rhs)
reduceInfixExpr Sub (ListValue lhs) (ListValue rhs) = return $ ListValue (lhs \\ rhs)
reduceInfixExpr Mul (IntValue lhs)  (IntValue rhs)  = return $ IntValue (lhs * rhs)
reduceInfixExpr Div (IntValue lhs)  (IntValue rhs)  = return $ IntValue (lhs `div` rhs)
reduceInfixExpr Eq  (BoolValue lhs) (BoolValue rhs) = return $ BoolValue (lhs == rhs)
reduceInfixExpr Eq  (IntValue lhs)  (IntValue rhs)  = return $ BoolValue (lhs == rhs)
reduceInfixExpr Eq  (ListValue lhs) (ListValue rhs) = return $ BoolValue (lhs == rhs)
reduceInfixExpr Eq  NilValue        NilValue        = return $ BoolValue True
reduceInfixExpr Eq  _               _               = return $ BoolValue False
reduceInfixExpr Neq lhs             rhs             = reduceInfixExpr Eq lhs rhs >>= return . BoolValue . not . bool
reduceInfixExpr Lt  (IntValue lhs) (IntValue rhs)   = return $ BoolValue (lhs < rhs)
reduceInfixExpr Lte (IntValue lhs) (IntValue rhs)   = return $ BoolValue (lhs <= rhs)
reduceInfixExpr Gt  (IntValue lhs) (IntValue rhs)   = return $ BoolValue (lhs > rhs)
reduceInfixExpr Gte (IntValue lhs) (IntValue rhs)   = return $ BoolValue (lhs >= rhs)

bool :: Value -> Bool
bool (BoolValue value) = value

