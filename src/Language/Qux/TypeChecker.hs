
module Language.Qux.TypeChecker (
    Env, Check,
    check
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

import Language.Qux.Ast
import Language.Qux.Exception
import Language.Qux.Util


type Env = State (Map Id [Type])

buildEnv :: Program -> Map Id [Type]
buildEnv (Program decls) = Map.fromList $ map (\(FunctionDecl name parameters _) -> (name, map fst parameters)) decls


type Check = ExceptT TypeException Env

check :: Program -> Maybe TypeException
check program = either Just (const Nothing) $ evalState (runExceptT $ checkProgram program) (buildEnv program)

checkProgram :: Program -> Check ()
checkProgram (Program decls)
    | null duplicateNames   = mapM_ checkDecl decls
    | otherwise             = throwError $ duplicateFunctionName (head duplicateNames)
    where
        functionNames   = map (\(FunctionDecl name _ _) -> name) decls
        duplicateNames  = functionNames \\ nub functionNames

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

checkExpr :: Expr -> Check Type
checkExpr (ApplicationExpr name arguments)  = do
    env <- get
    let expected = init $ env ! name

    case length expected == length arguments of
        True -> zipWithM expectExpr arguments $ map (:[]) expected
        False -> throwError $ invalidArgumentsCount (length expected) (length arguments)

    return $ head (env ! "@")
checkExpr (InfixExpr op lhs rhs)
    | op `elem` [Add, Sub]          = checkExpr lhs >>= \lhs' -> expectExpr rhs [lhs'] >>= expectType [IntType, ListType undefined]
    | op `elem` [Mul, Div]          = expectExpr lhs [IntType] >> expectExpr rhs [IntType]
    | op `elem` [Lt, Lte, Gt, Gte]  = expectExpr lhs [IntType] >> expectExpr rhs [IntType] *> return BoolType
    | op `elem` [Eq, Neq]           = ((:[]) <$> checkExpr lhs >>= expectExpr rhs) *> return BoolType
checkExpr (ListExpr elements)               = mapM checkExpr elements >>= checkElementTypes
checkExpr (ValueExpr value)                 = checkValue value

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

