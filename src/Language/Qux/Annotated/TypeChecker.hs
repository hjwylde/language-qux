
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
    -- * Environment
    Evaluation, Check,

    -- * Contexts
    Context, Locals,
    context,

    -- * Type checking

    -- ** Program checking
    check,

    -- ** Other node checking
    checkProgram, checkDecl, checkStmt, checkExpr, checkValue
) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import Data.Function (on)
import Data.List ((\\), nubBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe

import Language.Qux.Annotated.Exception
import Language.Qux.Annotated.Parser (SourcePos)
import Language.Qux.Annotated.Simplify
import qualified Language.Qux.Annotated.Syntax as Ann
import Language.Qux.Syntax


-- |    An environment that holds the global types (@Reader Context@) and the local types
--      (@Locals@).
type Evaluation = StateT Locals (Reader Context)

-- |    Either a 'TypeException' or an @a@.
--      Contains an underlying 'Evaluation' in the monad transformer.
type Check = ExceptT TypeException Evaluation


-- |    Global context that holds function definition types.
--      The function name and parameter types are held.
data Context = Context {
    functions :: Map Id [Type]
    }

-- | Local context.
type Locals = Map Id Type


-- | Returns a context for the given program.
context :: Program -> Context
context (Program decls) = Context { functions = Map.fromList $ map (\d -> (name d, types d)) decls }

retrieve :: Id -> Evaluation (Maybe [Type])
retrieve name = do
    maybeLocal  <- gets $ (fmap (:[])) . (Map.lookup name)
    maybeDef    <- asks $ (Map.lookup name) . functions

    return $ maybeLocal <|> maybeDef

once :: Monad m => MonadState s m => (s -> s) -> m a -> m a
once f m = get >>= \save -> modify f >> m <* put save


-- |    Type checks the program.
--      If an exception occurs then the result will be a 'TypeException', otherwise 'Nothing'.
--      This function wraps 'checkProgram' by building and evaluating the environment under
--      the hood.
check :: Ann.Program SourcePos -> Except TypeException ()
check program = mapExceptT
    (return
        . flip runReader (context $ sProgram program)
        . flip evalStateT Map.empty)
    (checkProgram program)

-- | Type checks a program.
checkProgram :: Ann.Program SourcePos -> Check ()
checkProgram (Ann.Program _ decls) = do
    when (not $ null duplicates) (throwError $ duplicateFunctionName (head duplicates))

    mapM_ checkDecl decls
    where
        duplicates = decls \\ nubBy ((==) `on` sId . Ann.name) decls

-- | Type checks a declaration.
checkDecl :: Ann.Decl SourcePos -> Check ()
checkDecl (Ann.FunctionDecl pos name parameters stmts) = do
    when (not $ null duplicates) (throwError $ duplicateParameterName (head $ map snd duplicates))

    once (Map.union $ Map.fromList (map (\(t, p) -> (sId p, sType t)) parameters)) (checkBlock stmts)
    where
        duplicates = parameters \\ nubBy ((==) `on` sId . snd) parameters

checkBlock :: [Ann.Stmt SourcePos] -> Check ()
checkBlock = mapM_ checkStmt

-- -- | Type checks a statement.
checkStmt :: Ann.Stmt SourcePos -> Check ()
checkStmt (Ann.IfStmt _ condition trueStmts falseStmts)   = do
    expectExpr condition [BoolType]

    checkBlock trueStmts
    checkBlock falseStmts
checkStmt (Ann.ReturnStmt _ expr)                         = do
    expected <- gets (! "@")

    void $ expectExpr expr [expected]
checkStmt (Ann.WhileStmt _ condition stmts)               = do
    expectExpr condition [BoolType]

    checkBlock stmts

-- | Type checks an expression.
checkExpr :: Ann.Expr SourcePos -> Check Type
checkExpr e@(Ann.ApplicationExpr pos name arguments)    = do
    maybeTypes <- lift $ retrieve (sId name)
    when (isNothing maybeTypes) (throwError $ undefinedFunctionCall e)

    let expected = init $ fromJust maybeTypes

    case length expected == length arguments of
        True    -> zipWithM expectExpr arguments $ map (:[]) expected
        False   -> throwError $ invalidArgumentsCount e (length expected)

    return $ last (fromJust maybeTypes)
checkExpr (Ann.BinaryExpr _ op lhs rhs)
    | op `elem` [Acc]               = do
    list <- expectExpr lhs [ListType undefined]
    let (ListType inner) = list in
        expectExpr rhs [IntType] >> return inner
    | op `elem` [Mul, Div, Mod]     = expectExpr lhs [IntType] >> expectExpr rhs [IntType]
    | op `elem` [Add, Sub]          = expectExpr lhs [IntType, ListType undefined] >>= (expectExpr rhs) . (:[])
    | op `elem` [Lt, Lte, Gt, Gte]  = expectExpr lhs [IntType] >> expectExpr rhs [IntType] >> return BoolType
    | op `elem` [Eq, Neq]           = ((:[]) <$> checkExpr lhs >>= expectExpr rhs) >> return BoolType
checkExpr (Ann.ListExpr _ [])                           = return $ ListType undefined
checkExpr (Ann.ListExpr _ elements)                     = do
    expected <- checkExpr $ head elements

    mapM_ (flip expectExpr [expected]) (tail elements) >> return expected
checkExpr (Ann.UnaryExpr _ op expr)
    | op `elem` [Len]               = expectExpr expr [ListType undefined] >> return IntType
    | op `elem` [Neg]               = expectExpr expr [IntType]
checkExpr (Ann.ValueExpr _ value)                       = checkValue value

-- -- | Type checks a value.
checkValue :: Value -> Check Type
checkValue (BoolValue _)        = return BoolType
checkValue (IntValue _)         = return IntType
checkValue NilValue             = return NilType


expectExpr :: Ann.Expr SourcePos -> [Type] -> Check Type
expectExpr expr expects = (attach (Ann.ann expr) <$> checkExpr expr) >>= flip expectType expects

expectType :: Ann.Type SourcePos -> [Type] -> Check Type
expectType received expects
    | sType received `elem` expects = return $ sType received
    | otherwise                     = throwError $ mismatchedType received expects

attach :: SourcePos -> Type -> Ann.Type SourcePos
attach pos BoolType         = Ann.BoolType pos
attach pos IntType          = Ann.IntType pos
attach pos (ListType inner) = Ann.ListType pos (attach undefined inner)
attach pos NilType          = Ann.NilType pos
