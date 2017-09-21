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

import Data.Map as Map

import LLVM.AST

data FunctionBuilder = FunctionBuilder
    { _currentBlockName :: Name
    , _blocks           :: Map Name BlockBuilder
    , _counter          :: Word
    }
    deriving (Eq, Show)

data BlockBuilder = BlockBuilder
    { _name     :: Name
    , _stack    :: [Named Instruction]
    , _term     :: Maybe (Named Terminator)
    }
    deriving (Eq, Show)

makeLenses ''FunctionBuilder

makeLenses ''BlockBuilder

newFunctionBuilder :: FunctionBuilder
newFunctionBuilder = FunctionBuilder
    { _currentBlockName = name
    , _blocks           = Map.singleton name (newBlockBuilder name)
    , _counter          = 1
    }
    where
        name = UnName 0

defaultBlockBuilder :: BlockBuilder
defaultBlockBuilder = BlockBuilder
    { _name     = error "no block name set"
    , _stack    = []
    , _term     = Nothing
    }

newBlockBuilder :: Name -> BlockBuilder
newBlockBuilder name' = defaultBlockBuilder & name .~ name'

getBlock :: MonadState FunctionBuilder m => m Name
getBlock = use currentBlockName

setBlock :: MonadState FunctionBuilder m => Name -> m ()
setBlock name = modify $ \s -> s & currentBlockName .~ name

addBlock :: MonadState FunctionBuilder m => Name -> m ()
addBlock name' = modify $ \s -> s & blocks %~ Map.insert name' (newBlockBuilder name')

modifyBlock :: MonadState FunctionBuilder m => (BlockBuilder -> BlockBuilder) -> m ()
modifyBlock f = currentBlock >>= \c -> modify (\s -> s & blocks %~ Map.insert (c ^. name) (f c))

currentBlock :: MonadState FunctionBuilder m => m BlockBuilder
currentBlock = getBlock >>= \name -> uses blocks (flip (Map.!) name)

append :: MonadState FunctionBuilder m => Named Instruction -> m ()
append instr = modifyBlock $ \b -> b & stack %~ (`snoc` instr)

terminate :: MonadState FunctionBuilder m => Named Terminator -> m ()
terminate term' = modifyBlock $ \b -> b & term .~ Just term'

freeUnName :: MonadState FunctionBuilder m => m Name
freeUnName = UnName <$> use counter <* (counter += 1)
