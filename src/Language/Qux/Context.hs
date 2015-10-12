
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
    localFunctions, importedFunctions,
) where

import              Data.Map    (Map, filterWithKey)
import qualified    Data.Map    as Map

import Language.Qux.Syntax


-- | Global context that holds function definition types.
data Context = Context {
    module_     :: [Id],                    -- ^ The current module identifier.
    functions   :: Map [Id] [(Type, Id)]    -- ^ A map of qualified identifiers to function types
                                            --   (including parameter names).
    }
    deriving (Eq, Show)


-- | Returns a base context for the given programs.
--   The base context populates @functions@ but not @module_@.
baseContext :: [Program] -> Context
baseContext programs = Context {
    module_     = [],
    functions   = Map.fromList $ [(module_ ++ [name], type_) | (Program module_ decls) <- programs, (FunctionDecl _ name type_ _) <- decls]
    }

-- | Returns a specific context for the given program.
context :: Program -> [Program] -> Context
context (Program m _) programs = (baseContext programs) { module_ = m }

-- | An empty context.
emptyContext :: Context
emptyContext = Context { module_ = [], functions = Map.empty }

-- | Gets the local functions from the context.
localFunctions :: Context -> Map [Id] [(Type, Id)]
localFunctions context = filterWithKey (\id _ -> init id == m) (functions context)
    where
        m = module_ context

-- | Gets the imported functions from the context.
importedFunctions :: Context -> Map [Id] [(Type, Id)]
importedFunctions context = filterWithKey (\id _ -> init id /= m) (functions context)
    where
        m = module_ context

