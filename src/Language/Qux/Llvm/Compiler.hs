
{-|
Module      : Language.Qux.Llvm.Compiler
Description : Compiles a Qux program into LLVM IR.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

A compiler that takes a 'Program' and outputs a LLVM 'Module'.
-}

module Language.Qux.Llvm.Compiler (
    -- * Compilation
    compile
) where

import Control.Monad.Reader
import Control.Monad.State

import              Data.List   (intercalate)
import              Data.Map    (Map)
import qualified    Data.Map    as Map
import              Data.Maybe
import              Data.Tuple  (swap)

import Language.Qux.Llvm.Builder    as B
import Language.Qux.Syntax          as Qux

import LLVM.General.AST                     as Llvm
import LLVM.General.AST.CallingConvention
import LLVM.General.AST.Constant            hiding (exact, nsw, nuw, operand0, operand1)
import LLVM.General.AST.Global              as G
import LLVM.General.AST.IntegerPredicate
import LLVM.General.AST.Type

import Prelude hiding (EQ)


data Context = Context {
    functions   :: Map Id [Qux.Type],
    locals      :: Map Id Qux.Type
    } deriving Show

context :: Program -> Context
context (Program _ decls) = Context {
    functions   = Map.fromList [(name, map fst parameters) | (FunctionDecl name parameters _) <- decls],
    locals      = Map.empty
    }


-- | Compiles the program into a LLVM 'Module'.
--   Generally speaking, compilation is done using the defaults.
--   Any exceptions to this will be clearly noted.
compile :: Program -> Module
compile program = runReader (compileProgram program) (context program)

compileProgram :: Program -> Reader Context Module
compileProgram (Program module_ decls) = do
    definitions <- mapM compileDecl decls

    return $ defaultModule {
        moduleName = intercalate "." module_,
        moduleDefinitions = definitions
        }

compileDecl :: Decl -> Reader Context Definition
compileDecl (FunctionDecl name' parameters stmts) = local (\r -> r { locals = Map.fromList $ map swap (init parameters) }) $ do
    blockBuilder <- execStateT (mapM_ compileStmt stmts) initialBuilder

    return $ GlobalDefinition functionDefaults {
        G.name          = Name name',
        G.returnType    = compileType $ fst (last parameters),
        G.parameters    = ([Parameter (compileType t) (Name p) [] | (t, p) <- init parameters], False),
        basicBlocks     = map (\b -> BasicBlock (B.name b) (stack b) (fromJust $ term b)) (Map.elems $ blocks blockBuilder)
        }

compileStmt :: Stmt -> StateT Builder (Reader Context) ()
compileStmt (IfStmt condition trueStmts falseStmts) = do
    operand <- compileExpr condition

    thenLabel <- Name <$> freeName
    elseLabel <- Name <$> freeName
    exitLabel <- Name <$> freeName

    terminate $ Do Llvm.CondBr { condition = operand, trueDest = thenLabel, falseDest = elseLabel, metadata' = [] }

    addBlock thenLabel >> setBlock thenLabel
    mapM_ compileStmt trueStmts

    c <- current
    when (isNothing $ term c) $ terminate (Do Llvm.Br { dest = exitLabel, metadata' = [] })

    addBlock elseLabel >> setBlock elseLabel
    mapM_ compileStmt falseStmts

    c <- current
    when (isNothing $ term c) $ terminate (Do Llvm.Br { dest = exitLabel, metadata' = [] })

    addBlock exitLabel >> setBlock exitLabel
    terminate $ Do Llvm.Unreachable { metadata' = [] }
compileStmt (ReturnStmt expr)                       = do
    operand <- compileExpr expr

    terminate $ Do Ret { returnOperand = Just operand, metadata' = [] }
compileStmt (WhileStmt condition stmts)             = do
    whileLabel  <- Name <$> freeName
    loopLabel   <- Name <$> freeName
    exitLabel   <- Name <$> freeName

    terminate $ Do Llvm.Br { dest = whileLabel, metadata' = [] }

    addBlock whileLabel >> setBlock whileLabel
    operand <- compileExpr condition
    terminate $ Do Llvm.CondBr { condition = operand, trueDest = loopLabel, falseDest = exitLabel, metadata' = [] }

    addBlock loopLabel >> setBlock loopLabel
    mapM_ compileStmt stmts

    c <- current
    when (isNothing $ term c) $ terminate (Do Llvm.Br { dest = whileLabel, metadata' = [] })

    addBlock exitLabel >> setBlock exitLabel
    terminate $ Do Llvm.Unreachable { metadata' = [] }

compileExpr :: Expr -> StateT Builder (Reader Context) Operand
compileExpr (TypedExpr type_ (ApplicationExpr name' arguments)) = do
    operands <- mapM compileExpr arguments

    n <- freeName

    asks (Map.member name' . locals) >>= \isLocal -> case isLocal of
        True    -> return $ LocalReference (compileType type_) (Name name')
        False   -> do
            append $ Name n := Call {
                isTailCall = False,
                Llvm.callingConvention = C,
                Llvm.returnAttributes = [],
                function = Right $ ConstantOperand $ GlobalReference (compileType type_) (Name name'),
                arguments = [(op, []) | op <- operands],
                Llvm.functionAttributes = [],
                metadata = []
                }

            return $ LocalReference (compileType type_) (Name n)
compileExpr (TypedExpr type_ (BinaryExpr op lhs rhs))           = do
    lhsOperand <- compileExpr lhs
    rhsOperand <- compileExpr rhs

    n <- freeName

    case op of
        Qux.Acc -> error "internal error: compilation for \"!!\" not implemented"
        Qux.Mul -> do
            append $ Name n := Llvm.Mul { nsw = False, nuw = False, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Div -> do
            append $ Name n := Llvm.SDiv { exact = False, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Mod -> do
            append $ Name n := Llvm.SRem { operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Add -> do
            append $ Name n := Llvm.Add { nsw = False, nuw = False, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Sub -> do
            append $ Name n := Llvm.Sub { nsw = False, nuw = False, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Lt  -> do
            append $ Name n := Llvm.ICmp { Llvm.iPredicate = SLT, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Lte -> do
            append $ Name n := Llvm.ICmp { Llvm.iPredicate = SLE, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Gt  -> do
            append $ Name n := Llvm.ICmp { Llvm.iPredicate = SGT, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Gte -> do
            append $ Name n := Llvm.ICmp { Llvm.iPredicate = SGE, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Eq  -> do
            append $ Name n := Llvm.ICmp { Llvm.iPredicate = EQ, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
        Qux.Neq -> do
            append $ Name n := Llvm.ICmp { Llvm.iPredicate = NE, operand0 = lhsOperand, operand1 = rhsOperand, metadata = [] }
            return $ LocalReference (compileType type_) (Name n)
compileExpr (TypedExpr _ (ListExpr _))                          = error "internal error: compilation for lists not implemented"
compileExpr (TypedExpr type_ (UnaryExpr op expr))               = do
    operand <- compileExpr expr

    n <- freeName

    case op of
        Len -> error "internal error: compilation for \"|..|\" not implemented"
        Neg -> append $ Name n := Llvm.Mul {
            nsw = False, nuw = False, metadata = [],
            operand0 = operand, operand1 = (ConstantOperand $ Int { integerBits = 32, integerValue = -1 })
            }

    return $ LocalReference (compileType type_) (Name n)
compileExpr (TypedExpr _ (ValueExpr value))                     = return $ ConstantOperand (compileValue value)
compileExpr _                                                   = error "internal error: cannot compile a non-typed expression (try applying type resolution)"

compileValue :: Value -> Constant
compileValue (BoolValue bool)   = Int {
    integerBits = 1,
    integerValue = toInteger (fromEnum bool)
    }
compileValue (IntValue int)     = Int {
    integerBits = 32,
    integerValue = toInteger int
    }
compileValue (ListValue _)      = error "internal error: compilation for lists not implemented"
compileValue NilValue           = error "internal error: compilation for nil not implemented"

compileType :: Qux.Type -> Llvm.Type
compileType BoolType        = i1
compileType IntType         = i32
compileType (ListType _)    = error "internal error: compilation for list types not implemented"
compileType NilType         = error "internal error: compilation for nil types not implemented"

