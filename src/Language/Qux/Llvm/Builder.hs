
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

module Language.Qux.Llvm.Builder where

import Control.Monad.State

import              Data.Map (Map)
import qualified    Data.Map as Map

import LLVM.General.AST


data Builder = Builder {
    currentBlock    :: Name,
    blocks          :: Map Name BlockBuilder,
    counter         :: Word
    }
    deriving (Eq, Show)

initialBuilder :: Builder
initialBuilder = Builder {
    currentBlock    = Name ".0",
    blocks          = Map.singleton (Name ".0") defaultBlockBuilder { name = Name ".0" },
    counter         = 1
    }

data BlockBuilder = BlockBuilder {
    name    :: Name,
    stack   :: [Named Instruction],
    term    :: Maybe (Named Terminator)
    }
    deriving (Eq, Show)

defaultBlockBuilder :: BlockBuilder
defaultBlockBuilder = BlockBuilder {
    name    = error "no block name set",
    stack   = [],
    term    = Nothing
    }

getBlock :: Monad m => MonadState Builder m => m Name
getBlock = gets currentBlock

setBlock :: Monad m => MonadState Builder m => Name -> m ()
setBlock name = modify $ \s -> s { currentBlock = name }

addBlock :: Monad m => MonadState Builder m => Name -> m ()
addBlock name' = modify $ \s -> s { blocks = Map.insert name' defaultBlockBuilder { name = name' } (blocks s) }

modifyBlock :: Monad m => MonadState Builder m => (BlockBuilder -> BlockBuilder) -> m ()
modifyBlock f = do
    c <- current
    modify $ \s -> s { blocks = Map.insert (name c) (f c) (blocks s) }

current :: Monad m => MonadState Builder m => m BlockBuilder
current = getBlock >>= \name -> gets (flip (Map.!) name . blocks)

append :: Monad m => MonadState Builder m => Named Instruction -> m ()
append instr = modifyBlock $ \b -> b { stack = stack b ++ [instr] }

terminate :: Monad m => MonadState Builder m => Named Terminator -> m ()
terminate term = modifyBlock $ \b -> b { term = Just term }

freeName :: Monad m => MonadState Builder m => m String
freeName = (("." ++) . show) <$> freeUnName

freeUnName :: Monad m => MonadState Builder m => m Word
freeUnName = gets counter <* modify (\s -> s { counter = counter s + 1 })

