{-|
Module      : Language.Qux.Llvm.Compiler
Description : Compiles a Qux program into LLVM IR.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

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

import qualified Data.Map   as Map
import           Data.Maybe

import Language.Qux.Context        hiding (local)
import Language.Qux.Llvm.Builder   as Builder
import Language.Qux.Llvm.Generator
import Language.Qux.Syntax         as Qux

import LLVM.General.AST                  as Llvm
import LLVM.General.AST.Constant         as Constant hiding (exact, nsw, nuw, operand0, operand1)
import LLVM.General.AST.Global           as Global
import LLVM.General.AST.IntegerPredicate

import Prelude hiding (EQ)

-- | Compiles the program into a LLVM 'Module'.
--   Generally speaking, compilation is done using the defaults.
--   Any exceptions to this will be clearly noted.
compileProgram :: Program -> Reader Context Module
compileProgram (Program module_ decls) = do
    importedFunctions' <- views (imported . functions) (map (\(id, type_) -> GlobalDefinition functionDefaults
        { Global.name       = Name $ mangle id
        , Global.returnType = compileType $ fst (last type_)
        , Global.parameters = ([Parameter (compileType t) (Name p) [] | (t, p) <- init type_], False)
        }) . Map.toList)

    let externalFunctions =
            [ GlobalDefinition functionDefaults
                { Global.name       = Name $ mangle (module_ ++ [name])
                , Global.returnType = compileType $ fst (last type_)
                , Global.parameters = ([Parameter (compileType t) (Name p) [] | (t, p) <- init type_], False)
                }
            | (FunctionDecl attrs name type_ _) <- decls, External `elem` attrs
            ]

    localFunctions <- mapM compileDecl
        [ decl
        | decl@(FunctionDecl attrs _ _ _) <- decls
        , External `notElem` attrs
        ]

    importedTypes' <- views (imported . types) (map $ \id -> TypeDefinition (Name $ mangle id) Nothing)

    let externalTypes =
            [ TypeDefinition (Name $ mangle (module_ ++ [name])) Nothing
            | (TypeDecl attrs name) <- decls
            , External `elem` attrs
            ]

    return $ defaultModule
        { moduleName        = qualify module_
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
        { Global.name       = Name $ mangle (module_' ++ [name])
        , Global.returnType = compileType $ fst (last type_)
        , Global.parameters = ([Parameter (compileType t) (Name p) [] | (t, p) <- init type_], False)
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
compileExpr (TypedExpr type_ (BinaryExpr Qux.Acc lhs rhs))  = compileExpr (TypedExpr type_ (CallExpr ["qux", "lang", "list", "acc"] [lhs, rhs]))
compileExpr (TypedExpr type_ (BinaryExpr op lhs rhs))       = do
    lhsOperand <- compileExpr lhs
    rhsOperand <- compileExpr rhs

    n <- freeName

    case op of
        Qux.Acc -> undefined
        Qux.Mul -> mul lhsOperand rhsOperand (Name n)
        Qux.Div -> sdiv lhsOperand rhsOperand (Name n)
        Qux.Mod -> srem lhsOperand rhsOperand (Name n)
        Qux.Add -> add lhsOperand rhsOperand (Name n)
        Qux.Sub -> sub lhsOperand rhsOperand (Name n)
        Qux.Lt  -> icmp SLT lhsOperand rhsOperand (Name n)
        Qux.Lte -> icmp SLE lhsOperand rhsOperand (Name n)
        Qux.Gt  -> icmp SGT lhsOperand rhsOperand (Name n)
        Qux.Gte -> icmp SGE lhsOperand rhsOperand (Name n)
        Qux.Eq  -> icmp EQ lhsOperand rhsOperand (Name n)
        Qux.Neq -> icmp NE lhsOperand rhsOperand (Name n)

    return $ local (compileType type_) (Name n)
compileExpr (TypedExpr type_ (CallExpr id arguments))       = do
    operands <- mapM compileExpr arguments

    n <- freeName

    call (compileType type_) (Name $ mangle id) operands (Name n)

    return $ local (compileType type_) (Name n)
compileExpr (TypedExpr type_ (ListExpr elements))           = compileExpr (TypedExpr type_ (CallExpr ["qux", "lang", "list", "new"] elements))
compileExpr (TypedExpr type_ (UnaryExpr Len expr))          = compileExpr (TypedExpr type_ (CallExpr ["qux", "lang", "list", "len"] [expr]))
compileExpr (TypedExpr type_ (UnaryExpr op expr))           = do
    operand <- compileExpr expr

    n <- freeName

    case op of
        Len -> undefined
        Neg -> mul operand (constant $ int (-1)) (Name n)

    return $ local (compileType type_) (Name n)
compileExpr (TypedExpr _ (ValueExpr value))                 = return $ constant (compileValue value)
compileExpr (TypedExpr type_ (VariableExpr name))           = return $ local (compileType type_) (Name name)
compileExpr _                                               = error "internal error: cannot compile a non-typed expression (try applying type resolution)"

compileValue :: Value -> Constant
compileValue (BoolValue True)   = true
compileValue (BoolValue False)  = false
compileValue (CharValue c)      = char c
compileValue (IntValue i)       = int i
compileValue (ListValue _)      = error "internal error: cannot compile a list as a constant"
compileValue NilValue           = nil

compileType :: Qux.Type -> Llvm.Type
compileType AnyType         = error "internal error: cannot compile an any type"
compileType BoolType        = boolType
compileType CharType        = charType
compileType IntType         = intType
compileType (ListType _)    = listType
compileType NilType         = nilType
