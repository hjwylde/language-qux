
{-|
Module      : Language.Qux.Context
Description : Context data type and utility methods.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Context data type and utility methods.
-}

module Language.Qux.Context (
    -- * Context
    Context(..),
    baseContext, context, emptyContext,
) where

import              Data.Map    (Map)
import qualified    Data.Map    as Map

import Language.Qux.Syntax


-- | Global context that holds function definition types.
data Context = Context {
    module_     :: [Id],            -- ^ The current module identifier.
    functions   :: Map [Id] [Type]  -- ^ A map of qualified identifiers to parameter types.
    }
    deriving (Eq, Show)


-- | Returns a base context for the given programs.
--   The base context populates @functions@ but not @module_@.
baseContext :: [Program] -> Context
baseContext programs = Context {
    module_     = [],
    functions   = Map.fromList $ [(module_ ++ [name], map fst parameters) | (Program module_ decls) <- programs, (FunctionDecl name parameters _) <- decls]
    }

-- | Returns a specific context for the given program.
context :: Program -> [Program] -> Context
context (Program m _) programs = (baseContext programs) { module_ = m }

-- | An empty context.
emptyContext :: Context
emptyContext = Context { module_ = [], functions = Map.empty }

