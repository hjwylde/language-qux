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

import Data.Char

import LLVM.AST as Llvm
import LLVM.AST.CallingConvention
import LLVM.AST.Constant as Constant hiding (address, exact, indices, inBounds, iPredicate, nsw, nuw, operand0, operand1, type')
import LLVM.AST.Type as Type
import LLVM.AST.AddrSpace
import LLVM.AST.IntegerPredicate
import LLVM.AST.Global           as Global hiding (alignment, callingConvention, functionAttributes, returnAttributes, type')

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

return_ :: MonadState FunctionBuilder m => WriterT BlockBuilder m (Maybe Operand) -> m ()
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

alloca :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Type -> Maybe Operand -> Name -> m ()
alloca type_ mNumElements name = append $ name := Llvm.Alloca
    { allocatedType = type_
    , numElements   = mNumElements
    , alignment     = 0
    , metadata      = []
    }

bitcast :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Type -> Name -> m ()
bitcast operand type_ name = append $ name := Llvm.BitCast
    { operand0  = operand
    , type'     = type_
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

store :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Operand -> Operand -> m ()
store valueOperand addressOperand = do
    unused <- freeUnName

    append $ unused := Llvm.Store
        { volatile          = False
        , address           = addressOperand
        , value             = valueOperand
        , maybeAtomicity    = Nothing
        , alignment         = 0
        , metadata          = []
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

ret :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Maybe Operand -> m ()
ret mOperand = terminate $ Do Ret
    { returnOperand = mOperand
    , metadata'     = []
    }

unreachable :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => m ()
unreachable = terminate $ Do Unreachable { metadata' = [] }

-- Constants

char :: Char -> Constant
char c = Int
    { integerBits   = 8
    , integerValue  = toInteger $ ord c
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

str :: String -> Constant
str s = Array
    { memberType    = charType
    , memberValues  = map char s
    }

true :: Constant
true = Int
    { integerBits   = 1
    , integerValue  = 1
    }

-- Types

arrayType :: Type -> Integer -> Type
arrayType type_ numElements = ArrayType
    { nArrayElements    = fromIntegral numElements
    , elementType       = type_
    }

boolType :: Type
boolType = i1

charType :: Type
charType = i8

intType :: Type
intType = i32

ptrType :: Type -> Type
ptrType type_ = PointerType
    { pointerReferent   = type_
    , pointerAddrSpace  = AddrSpace 0
    }

strType :: Type
strType = ptrType i8

voidType :: Type
voidType = VoidType
