{-|
Module      : Language.Qux.Llvm.Compiler
Description : Compiles a Qux program into LLVM IR.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

A compiler that takes a 'Program' and outputs a LLVM 'Module'.
-}

module Language.Qux.Llvm.Compiler (
    -- * Global context
    Context(..),
    baseContext, context, emptyContext,

    -- * Compilation
    compileProgram,
) where

import Control.Lens         hiding (Context)
import Control.Monad.Reader hiding (local)
import Control.Monad.State

import qualified Data.Map    as Map
import           Data.Maybe
import           Data.String

import Language.Qux.Context        hiding (local)
import Language.Qux.Llvm.Builder   as Builder
import Language.Qux.Llvm.Generator
import Language.Qux.Syntax         as Qux

import LLVM.AST                  as Llvm
import LLVM.AST.Constant         as Constant hiding (exact, nsw, nuw, operand0, operand1)
import LLVM.AST.Global           as Global
import LLVM.AST.IntegerPredicate

import Prelude hiding (EQ)

-- | Compiles the program into a LLVM 'Module'.
--   Generally speaking, compilation is done using the defaults.
--   Any exceptions to this will be clearly noted.
compileProgram :: Program -> Reader Context Module
compileProgram (Program module_ decls) = do
    importedFunctions' <- views (imported . functions) (map (\(id, type_) -> GlobalDefinition functionDefaults
        { Global.name       = mkName $ mangle id
        , Global.returnType = compileType $ fst (last type_)
        , Global.parameters = ([Parameter (compileType t) (mkName p) [] | (t, p) <- init type_], False)
        }) . Map.toList)

    let externalFunctions =
            [ GlobalDefinition functionDefaults
                { Global.name       = mkName $ mangle (module_ ++ [name])
                , Global.returnType = compileType $ fst (last type_)
                , Global.parameters = ([Parameter (compileType t) (mkName p) [] | (t, p) <- init type_], False)
                }
            | (FunctionDecl attrs name type_ _) <- decls, External `elem` attrs
            ]

    localFunctions <- mapM compileDecl
        [ decl
        | decl@(FunctionDecl attrs _ _ _) <- decls
        , External `notElem` attrs
        ]

    importedTypes' <- views (imported . types) (map $ \id -> TypeDefinition (mkName $ mangle id) Nothing)

    let externalTypes =
            [ TypeDefinition (mkName $ mangle (module_ ++ [name])) Nothing
            | (TypeDecl attrs name) <- decls
            , External `elem` attrs
            ]

    return $ defaultModule
        { moduleName        = fromString $ qualify module_
        , moduleDefinitions = concat
            [ importedFunctions'
            , externalFunctions
            , localFunctions
            , importedTypes'
            , externalTypes
            ]
        }

compileDecl :: Decl -> Reader Context Definition
compileDecl (FunctionDecl _ name type_ stmts)   = do
    module_'        <- view module_
    blockBuilder    <- execStateT (mapM_ compileStmt stmts) initialBuilder

    return $ GlobalDefinition functionDefaults
        { Global.name       = mkName $ mangle (module_' ++ [name])
        , Global.returnType = compileType $ fst (last type_)
        , Global.parameters = ([Parameter (compileType t) (mkName p) [] | (t, p) <- init type_], False)
        , basicBlocks       = map (\b -> BasicBlock (b ^. Builder.name) (b ^. stack) (fromJust $ b ^. term)) (Map.elems $ blockBuilder ^. blocks)
        }
compileDecl (ImportDecl _)                      = error "internal error: cannot compile an import declaration"
compileDecl (TypeDecl _ _)                      = error "internal error: cannot compile a type declaration"

compileStmt :: Stmt -> StateT Builder (Reader Context) ()
compileStmt (IfStmt condition trueStmts falseStmts) = do
    operand <- compileExpr condition

    if_ operand
        (mapM_ compileStmt trueStmts)
        (mapM_ compileStmt falseStmts)
compileStmt (ReturnStmt expr)                       = do
    operand <- compileExpr expr

    ret operand
compileStmt (WhileStmt condition stmts)             = do
    while (compileExpr condition)
        (mapM_ compileStmt stmts)

compileExpr :: Expr -> StateT Builder (Reader Context) Operand
compileExpr (TypedExpr type_ (BinaryExpr op lhs rhs)) = do
    lhsOperand <- compileExpr lhs
    rhsOperand <- compileExpr rhs

    name <- freeUnName

    case op of
        Qux.Mul -> mul lhsOperand rhsOperand name
        Qux.Div -> sdiv lhsOperand rhsOperand name
        Qux.Mod -> srem lhsOperand rhsOperand name
        Qux.Add -> add lhsOperand rhsOperand name
        Qux.Sub -> sub lhsOperand rhsOperand name
        Qux.Lt  -> icmp SLT lhsOperand rhsOperand name
        Qux.Lte -> icmp SLE lhsOperand rhsOperand name
        Qux.Gt  -> icmp SGT lhsOperand rhsOperand name
        Qux.Gte -> icmp SGE lhsOperand rhsOperand name
        Qux.Eq  -> icmp EQ lhsOperand rhsOperand name
        Qux.Neq -> icmp NE lhsOperand rhsOperand name

    return $ local (compileType type_) name
compileExpr (TypedExpr type_ (CallExpr id arguments))       = do
    operands <- mapM compileExpr arguments

    name <- freeUnName

    call (compileType type_) (mkName $ mangle id) operands name

    return $ local (compileType type_) name
compileExpr (TypedExpr type_ (UnaryExpr op expr))           = do
    operand <- compileExpr expr

    name <- freeUnName

    case op of
        Neg -> mul operand (constant $ int (-1)) name

    return $ local (compileType type_) name
compileExpr (TypedExpr _ (ValueExpr value))                 = return $ constant (compileValue value)
compileExpr (TypedExpr type_ (VariableExpr name))           = return $ local (compileType type_) (mkName name)
compileExpr _                                               = error "internal error: cannot compile a non-typed expression (try applying type resolution)"

compileValue :: Value -> Constant
compileValue (BoolValue True)   = true
compileValue (BoolValue False)  = false
compileValue (IntValue i)       = int i
compileValue NilValue           = nil
compileValue (StrValue str)     = string str

compileType :: Qux.Type -> Llvm.Type
compileType AnyType     = error "internal error: cannot compile an any type"
compileType BoolType    = boolType
compileType IntType     = intType
compileType NilType     = nilType
compileType StrType     = strType
