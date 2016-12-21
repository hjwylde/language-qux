{-|
Module      : Language.Qux.Llvm.Builder
Description : Build utilities for the LLVM compiler.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Build utilities for the LLVM compiler, see "Compiler".
-}

{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Qux.Llvm.Builder where

import Control.Lens
import Control.Monad.State

import Data.Map as Map

import LLVM.General.AST

data Builder = Builder
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

makeLenses ''Builder

makeLenses ''BlockBuilder

initialBuilder :: Builder
initialBuilder = Builder
    { _currentBlockName = name'
    , _blocks           = Map.singleton name' (newBlockBuilder name')
    , _counter          = 1
    }
    where
        name' = Name ".0"

defaultBlockBuilder :: BlockBuilder
defaultBlockBuilder = BlockBuilder
    { _name     = error "no block name set"
    , _stack    = []
    , _term     = Nothing
    }

newBlockBuilder :: Name -> BlockBuilder
newBlockBuilder name' = defaultBlockBuilder & name .~ name'

getBlock :: MonadState Builder m => m Name
getBlock = use currentBlockName

setBlock :: MonadState Builder m => Name -> m ()
setBlock name = modify $ \s -> s & currentBlockName .~ name

addBlock :: MonadState Builder m => Name -> m ()
addBlock name' = modify $ \s -> s & blocks %~ Map.insert name' (newBlockBuilder name')

modifyBlock :: MonadState Builder m => (BlockBuilder -> BlockBuilder) -> m ()
modifyBlock f = currentBlock >>= \c -> modify (\s -> s & blocks %~ Map.insert (c ^. name) (f c))

currentBlock :: MonadState Builder m => m BlockBuilder
currentBlock = getBlock >>= \name -> uses blocks (flip (Map.!) name)

append :: MonadState Builder m => Named Instruction -> m ()
append instr = modifyBlock $ \b -> b & stack %~ (`snoc` instr)

terminate :: MonadState Builder m => Named Terminator -> m ()
terminate term' = modifyBlock $ \b -> b & term .~ Just term'

freeName :: MonadState Builder m => m String
freeName = (('.':) . show) <$> freeUnName

freeUnName :: MonadState Builder m => m Word
freeUnName = use counter <* (counter += 1)
