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

import Control.Lens
import Control.Monad.State

import Data.Maybe

import LLVM.AST as Llvm
import LLVM.AST.CallingConvention
import LLVM.AST.Constant as Constant hiding (exact, nsw, nuw, operand0, operand1, iPredicate)
import LLVM.AST.Type as Type
import LLVM.AST.IntegerPredicate

import Language.Qux.Llvm.Builder

-- Definitions

--function :: 

--type :: TypeDefinition

-- Operands

constant :: Constant -> Operand
constant = ConstantOperand

local :: Type -> Name -> Operand
local = LocalReference

global :: Type -> Name -> Operand
global type_ = constant . GlobalReference type_

-- Control flow

if_ :: MonadState FunctionBuilder m => Operand -> m () -> m () -> m ()
if_ operand mTrueInstrs mFalseInstrs = do
    thenLabel <- freeUnName
    elseLabel <- freeUnName
    exitLabel <- freeUnName

    condBr operand thenLabel elseLabel

    addBlock thenLabel >> setBlock thenLabel
    mTrueInstrs

    c <- currentBlock
    when (isNothing $ c ^. term) $ br exitLabel

    addBlock elseLabel >> setBlock elseLabel
    mFalseInstrs

    c <- currentBlock
    when (isNothing $ c ^. term) $ br exitLabel

    addBlock exitLabel >> setBlock exitLabel
    unreachable

while :: MonadState FunctionBuilder m => m Operand -> m () -> m ()
while mOperand mInstrs = do
    whileLabel  <- freeUnName
    loopLabel   <- freeUnName
    exitLabel   <- freeUnName

    br whileLabel

    addBlock whileLabel >> setBlock whileLabel
    operand <- mOperand
    condBr operand loopLabel exitLabel

    addBlock loopLabel >> setBlock loopLabel
    mInstrs

    c <- currentBlock
    when (isNothing $ c ^. term) $ br whileLabel

    addBlock exitLabel >> setBlock exitLabel
    unreachable

-- Instructions

add :: MonadState FunctionBuilder m => Operand -> Operand -> Name -> m ()
add lhsOperand rhsOperand name = append $ name := Llvm.Add
    { nsw       = False
    , nuw       = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

call :: MonadState FunctionBuilder m => Type -> Name -> [Operand] -> Name -> m ()
call type_ function operands name = append $ name := Call
    { tailCallKind          = Nothing
    , callingConvention     = C
    , returnAttributes      = []
    , function              = Right $ global type_ function
    , arguments             = [(op, []) | op <- operands]
    , functionAttributes    = []
    , metadata              = []
    }

icmp :: MonadState FunctionBuilder m => IntegerPredicate -> Operand -> Operand -> Name -> m ()
icmp predicate lhsOperand rhsOperand name = append $ name := Llvm.ICmp
    { iPredicate    = predicate
    , operand0      = lhsOperand
    , operand1      = rhsOperand
    , metadata      = []
    }

mul :: MonadState FunctionBuilder m => Operand -> Operand -> Name -> m ()
mul lhsOperand rhsOperand name = append $ name := Llvm.Mul
    { nsw       = False
    , nuw       = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

sdiv :: MonadState FunctionBuilder m => Operand -> Operand -> Name -> m ()
sdiv lhsOperand rhsOperand name = append $ name := Llvm.SDiv
    { exact     = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

srem :: MonadState FunctionBuilder m => Operand -> Operand -> Name -> m ()
srem lhsOperand rhsOperand name = append $ name := Llvm.SRem
    { operand0 = lhsOperand
    , operand1 = rhsOperand
    , metadata = []
    }

sub :: MonadState FunctionBuilder m => Operand -> Operand -> Name -> m ()
sub lhsOperand rhsOperand name = append $ name := Llvm.Sub
    { nsw       = False
    , nuw       = False
    , operand0  = lhsOperand
    , operand1  = rhsOperand
    , metadata  = []
    }

-- Terminators

br :: MonadState FunctionBuilder m => Name -> m ()
br label = terminate $ Do Br
    { dest      = label
    , metadata' = []
    }

condBr :: MonadState FunctionBuilder m => Operand -> Name -> Name -> m ()
condBr operand trueLabel falseLabel = terminate $ Do CondBr
    { condition = operand
    , trueDest  = trueLabel
    , falseDest = falseLabel
    , metadata' = []
    }

ret :: MonadState FunctionBuilder m => Operand -> m ()
ret operand = terminate $ Do Ret
    { returnOperand = Just operand
    , metadata'     = []
    }

unreachable :: MonadState FunctionBuilder m => m ()
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
