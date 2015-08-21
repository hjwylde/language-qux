
{-|
Module      : Language.Qux.Util
Description : Utility functions for this package.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Utility functions for this package.
-}

module Language.Qux.Util where

import Control.Monad.State


-- * State

-- |    Runs the given 'State' monad @r@, with a modified state (from applying @f@).
--      The original---non-modified---state is returned.
runStateWith :: State s a -> (s -> s) -> State s a
runStateWith r f = state $ \s -> (evalState r (f s), s)

