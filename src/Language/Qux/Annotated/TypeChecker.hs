
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
    Check,
    runCheck, execCheck,

    -- * Global context
    Context,
    context, emptyContext,

    -- * Local context
    Locals,

    -- * Type checking
    check, checkProgram, checkDecl, checkStmt, checkExpr, checkValue
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import              Data.Function   (on)
import              Data.List       ((\\), nubBy)
import              Data.Map        (Map, (!))
import qualified    Data.Map        as Map

import              Language.Qux.Annotated.Exception
import              Language.Qux.Annotated.Parser       (SourcePos)
import              Language.Qux.Annotated.Simplify
import qualified    Language.Qux.Annotated.Syntax       as Ann
import              Language.Qux.Syntax


-- |    A type that allows collecting errors while type checking a program.
--      Requires a 'Context' for evaluation.
type Check = ReaderT Context (Writer [TypeException])

-- | Runs the given check with the context.
runCheck :: Check a -> Context -> (a, [TypeException])
runCheck check context = runWriter $ runReaderT check context

execCheck :: Check a -> Context -> [TypeException]
execCheck check context = execWriter $ runReaderT check context


-- |    Global context that holds function definition types.
--      The function name and parameter types are held.
data Context = Context {
    functions :: Map Id [Type]
    }

-- | Returns a context for the given program.
context :: Program -> Context
context (Program _ decls) = Context { functions = Map.fromList $ map (\d -> (name d, types d)) decls }

-- | An empty context.
emptyContext :: Context
emptyContext = Context { functions = Map.empty }


-- | Local context.
type Locals = Map Id Type

retrieve :: Id -> StateT Locals Check (Maybe [Type])
retrieve name = do
    maybeLocal  <- gets $ (fmap (:[])) . (Map.lookup name)
    maybeDef    <- asks $ (Map.lookup name) . functions

    return $ maybeLocal <|> maybeDef


-- |    Type checks the program, returning any errors that are found.
--      A result of @[]@ indicates the program is well-typed.
check :: Ann.Program SourcePos -> [TypeException]
check program = execCheck (checkProgram program) (context $ sProgram program)

-- | Type checks a program.
checkProgram :: Ann.Program SourcePos -> Check ()
checkProgram (Ann.Program _ _ decls)
    | null duplicates   = mapM_ checkDecl decls
    | otherwise         = tell $ map duplicateFunctionName duplicates
    where
        duplicates = decls \\ nubBy ((==) `on` sId . Ann.name) decls

-- | Type checks a declaration.
checkDecl :: Ann.Decl SourcePos -> Check ()
checkDecl (Ann.FunctionDecl _ _ parameters stmts)
    | null duplicates   = evalStateT (checkBlock stmts) (Map.fromList (map (\(t, p) -> (sId p, sType t)) parameters))
    | otherwise         = tell $ map (duplicateParameterName . snd) duplicates
    where
        duplicates = parameters \\ nubBy ((==) `on` sId . snd) parameters

checkBlock :: [Ann.Stmt SourcePos] -> StateT Locals Check ()
checkBlock = mapM_ checkStmt

-- -- | Type checks a statement.
checkStmt :: Ann.Stmt SourcePos -> StateT Locals Check ()
checkStmt (Ann.IfStmt _ condition trueStmts falseStmts)   = do
    expectExpr_ condition [BoolType]

    checkBlock trueStmts
    checkBlock falseStmts
checkStmt (Ann.ReturnStmt _ expr)                         = do
    expected <- gets (! "@")

    void $ expectExpr expr [expected]
checkStmt (Ann.WhileStmt _ condition stmts)               = do
    expectExpr_ condition [BoolType]

    checkBlock stmts

-- | Type checks an expression.
checkExpr :: Ann.Expr SourcePos -> StateT Locals Check Type
checkExpr e@(Ann.ApplicationExpr _ name arguments)      = retrieve (sId name) >>= maybe
    (tell [undefinedFunctionCall e] >> return (error "internal error: undefined function call has no type"))
    (\types -> do
        let expected = init types

        zipWithM_ expectExpr arguments $ map (:[]) expected

        when (length expected /= length arguments) $ tell [invalidArgumentsCount e (length expected)]

        return $ last types)
checkExpr (Ann.BinaryExpr _ op lhs rhs)
    | op `elem` [Acc]               = do
        list <- expectExpr lhs [ListType $ error "internal error: top type not implemented"]

        let (ListType inner) = list in
            expectExpr rhs [IntType] >> return inner
    | op `elem` [Mul, Div, Mod]     = expectExpr lhs [IntType] >> expectExpr rhs [IntType]
    | op `elem` [Add, Sub]          = expectExpr lhs [IntType, ListType $ error "internal error: top type not implemented"] >>= (expectExpr rhs) . (:[])
    | op `elem` [Lt, Lte, Gt, Gte]  = expectExpr lhs [IntType] >> expectExpr rhs [IntType] >> return BoolType
    | op `elem` [Eq, Neq]           = ((:[]) <$> checkExpr lhs >>= expectExpr rhs) >> return BoolType
    | otherwise                     = error $ "internal error: " ++ show op ++ " not implemented"
checkExpr (Ann.ListExpr _ [])                           = return $ ListType (error "internal error: top type not implemented")
checkExpr (Ann.ListExpr _ elements)                     = do
    expected <- checkExpr $ head elements

    mapM_ (flip expectExpr [expected]) (tail elements)

    return $ ListType expected
checkExpr (Ann.UnaryExpr _ op expr)
    | op `elem` [Len]               = expectExpr expr [ListType $ error "internal error: top type not implemented"] >> return IntType
    | op `elem` [Neg]               = expectExpr expr [IntType]
    | otherwise                     = error $ "internal error: " ++ show op ++ " not implemented"
checkExpr (Ann.ValueExpr _ value)                       = lift $ checkValue value

-- | Type checks a value.
checkValue :: Value -> Check Type
checkValue (BoolValue _)        = return BoolType
checkValue (IntValue _)         = return IntType
checkValue (ListValue [])       = return $ ListType (error "internal error: top type not implemented")
checkValue (ListValue elements) = do
    expected <- checkValue $ head elements

    mapM_ (flip expectValue [expected]) (tail elements)

    return $ ListType expected
checkValue NilValue             = return NilType


expectExpr :: Ann.Expr SourcePos -> [Type] -> StateT Locals Check Type
expectExpr expr expects = do
    type_ <- (attach (Ann.ann expr) <$> checkExpr expr)

    lift $ expectType type_ expects

expectExpr_ :: Ann.Expr SourcePos -> [Type] -> StateT Locals Check ()
expectExpr_ = fmap void . expectExpr

expectValue :: Value -> [Type] -> Check Type
expectValue value expects = (attach undefined <$> checkValue value) >>= flip expectType expects

expectType :: Ann.Type SourcePos -> [Type] -> Check Type
expectType received expects
    | sType received `elem` expects = return $ sType received
    | otherwise                     = tell [mismatchedType received expects] >> return (sType received)

attach :: SourcePos -> Type -> Ann.Type SourcePos
attach pos BoolType         = Ann.BoolType pos
attach pos IntType          = Ann.IntType  pos
attach pos (ListType inner) = Ann.ListType pos (attach undefined inner)
attach pos NilType          = Ann.NilType  pos

