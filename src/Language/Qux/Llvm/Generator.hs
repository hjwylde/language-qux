{-|
Module      : Language.Qux.Llvm.Generator
Description : Generation utilities for the LLVM compiler.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

Generation utilities for the LLVM compiler, see "Compiler".
-}

{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE FlexibleContexts #-}

module Language.Qux.Llvm.Generator where

import Control.Monad.State
import Control.Monad.Writer

import LLVM.AST as Llvm
import LLVM.AST.CallingConvention
import LLVM.AST.Constant as Constant hiding (exact, nsw, nuw, operand0, operand1, iPredicate)
import LLVM.AST.Type as Type
import LLVM.AST.IntegerPredicate
import LLVM.AST.Global           as Global hiding (callingConvention, functionAttributes, returnAttributes)

import Language.Qux.Llvm.Builder

-- Definitions

function :: Name -> Type -> [(Type, Name)] -> [BasicBlock] -> Definition
function name type_ parameters blocks = GlobalDefinition functionDefaults
        { Global.name       = name
        , Global.returnType = type_
        , Global.parameters = ([Parameter type_ name [] | (type_, name) <- parameters], False)
        , basicBlocks       = blocks
        }

-- Operands

constant :: Constant -> Operand
constant = ConstantOperand

local :: Type -> Name -> Operand
local = LocalReference

global :: Type -> Name -> Operand
global type_ = constant . GlobalReference type_

-- Control flow

if_ :: MonadState FunctionBuilder m => WriterT BlockBuilder m Operand -> WriterT BlockBuilder m () -> WriterT BlockBuilder m () -> m ()
if_ mCondition mTrueInstrs mFalseInstrs = do
    thenLabel <- freeUnName
    elseLabel <- freeUnName
    exitLabel <- freeUnName

    withCurrentBlock $ do
        operand <- mCondition

        condBr operand thenLabel elseLabel

    commitCurrentBlock

    withNewBlock thenLabel $ do
        mTrueInstrs

        br exitLabel

    commitCurrentBlock

    withNewBlock elseLabel $ do
        mFalseInstrs

        br exitLabel

    commitCurrentBlock

    createNewBlock exitLabel

invoke :: MonadState FunctionBuilder m => WriterT BlockBuilder m () -> m ()
invoke mExpr = withCurrentBlock mExpr

return_ :: MonadState FunctionBuilder m => WriterT BlockBuilder m Operand -> m ()
return_ mExpr = withCurrentBlock $ mExpr >>= ret

while :: MonadState FunctionBuilder m => WriterT BlockBuilder m Operand -> WriterT BlockBuilder m () -> m ()
while mOperand mInstrs = do
    whileLabel  <- freeUnName
    loopLabel   <- freeUnName
    exitLabel   <- freeUnName

    withCurrentBlock $ do
        br whileLabel

    commitCurrentBlock

    withNewBlock whileLabel $ do
        operand <- mOperand

        condBr operand loopLabel exitLabel

    commitCurrentBlock

    withNewBlock loopLabel $ do
        mInstrs

        br whileLabel

    commitCurrentBlock

    createNewBlock exitLabel

-- Instructions

add :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Operand -> Name -> m ()
add lhsOperand rhsOperand name = append $ name := Llvm.Add
    { nsw       = False
    , nuw       = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

call :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Type -> Name -> [Operand] -> Name -> m ()
call type_ function operands name = append $ name := Call
    { tailCallKind          = Nothing
    , callingConvention     = C
    , returnAttributes      = []
    , Llvm.function         = Right $ global type_ function
    , arguments             = [(op, []) | op <- operands]
    , functionAttributes    = []
    , metadata              = []
    }

icmp :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => IntegerPredicate -> Operand -> Operand -> Name -> m ()
icmp predicate lhsOperand rhsOperand name = append $ name := Llvm.ICmp
    { iPredicate    = predicate
    , operand0      = lhsOperand
    , operand1      = rhsOperand
    , metadata      = []
    }

mul :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Operand -> Name -> m ()
mul lhsOperand rhsOperand name = append $ name := Llvm.Mul
    { nsw       = False
    , nuw       = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

sdiv :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Operand -> Name -> m ()
sdiv lhsOperand rhsOperand name = append $ name := Llvm.SDiv
    { exact     = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

srem :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Operand -> Name -> m ()
srem lhsOperand rhsOperand name = append $ name := Llvm.SRem
    { operand0 = lhsOperand
    , operand1 = rhsOperand
    , metadata = []
    }

sub :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Operand -> Name -> m ()
sub lhsOperand rhsOperand name = append $ name := Llvm.Sub
    { nsw       = False
    , nuw       = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

-- Terminators

br :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Name -> m ()
br label = terminate $ Do Br
    { dest      = label
    , metadata' = []
    }

condBr :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Name -> Name -> m ()
condBr operand trueLabel falseLabel = terminate $ Do CondBr
    { condition = operand
    , trueDest  = trueLabel
    , falseDest = falseLabel
    , metadata' = []
    }

ret :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> m ()
ret operand = terminate $ Do Ret
    { returnOperand = Just operand
    , metadata'     = []
    }

unreachable :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => m ()
unreachable = terminate $ Do Unreachable { metadata' = [] }

-- Constants

true :: Constant
true = Int
    { integerBits   = 1
    , integerValue  = 1
    }

false :: Constant
false = Int
    { integerBits   = 1
    , integerValue  = 0
    }

int :: Integer -> Constant
int i = Int
    { integerBits   = 32
    , integerValue  = i
    }

nil :: Constant
nil = Struct
    { structName        = Nothing
    , Constant.isPacked = True
    , memberValues      = []
    }

str :: String -> Constant
str _ = undefined

-- Types

boolType :: Type
boolType = i1

intType :: Type
intType = i32

nilType :: Type
nilType = StructureType
    { Type.isPacked = True
    , elementTypes  = []
    }

strType :: Type
strType = undefined
