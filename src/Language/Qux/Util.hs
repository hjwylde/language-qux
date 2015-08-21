
module Language.Qux.Util where

import Control.Monad.State


runStateWith :: State s a -> (s -> s) -> State s a
runStateWith r f = state $ \s -> (evalState r (f s), s)

