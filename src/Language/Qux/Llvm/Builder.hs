{-|
Module      : Language.Qux.Llvm.Builder
Description : Build utilities for the LLVM compiler.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

Build utilities for the LLVM compiler, see "Compiler".
-}

{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Qux.Llvm.Builder where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe

import LLVM.AST

data FunctionBuilder = FunctionBuilder
    { _currentBlock :: (Name, BlockBuilder)
    , _blocks       :: [BasicBlock]
    , _counter      :: Word
    }
    deriving (Eq, Show)

data BlockBuilder = BlockBuilder
    { _stack    :: [Named Instruction]
    , _term     :: First (Named Terminator)
    }
    deriving (Eq, Show)

makeLenses ''FunctionBuilder

makeLenses ''BlockBuilder

instance Monoid BlockBuilder where
    mempty = BlockBuilder mempty mempty

    mappend a b = a
        & stack <>~ b ^. stack
        & term <>~ b ^. term

newFunctionBuilder :: FunctionBuilder
newFunctionBuilder = FunctionBuilder
    { _currentBlock = (name, mempty)
    , _blocks       = []
    , _counter      = 1
    }
    where
        name = UnName 0

buildBlock :: Name -> BlockBuilder -> BasicBlock
buildBlock name (BlockBuilder stack term) = BasicBlock name stack (fromJust $ getFirst term)

withCurrentBlock :: MonadState FunctionBuilder m => WriterT BlockBuilder m a -> m a
withCurrentBlock instrs = do
    (result, instrs') <- runWriterT instrs

    currentBlock . _2 <>= instrs'

    return result

commitCurrentBlock :: MonadState FunctionBuilder m => m ()
commitCurrentBlock = do
    basicBlock <- uses currentBlock $ uncurry buildBlock

    blocks <>= [basicBlock]

    name <- freeUnName
    currentBlock .= (name, mempty)

createNewBlock :: MonadState FunctionBuilder m => Name -> m ()
createNewBlock name = currentBlock .= (name, mempty)

withNewBlock :: MonadState FunctionBuilder m => Name -> WriterT BlockBuilder m a -> m a
withNewBlock name instrs = createNewBlock name >> withCurrentBlock instrs

append :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Named Instruction -> m ()
append instr = scribe stack [instr]

terminate :: (MonadState FunctionBuilder m, MonadWriter BlockBuilder m) => Named Terminator -> m ()
terminate instr = scribe term (First $ Just instr)

freeUnName :: MonadState FunctionBuilder m => m Name
freeUnName = UnName <$> use counter <* (counter += 1)
