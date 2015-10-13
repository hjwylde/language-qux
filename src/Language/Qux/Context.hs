
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

    -- ** Creating contexts
    emptyContext, baseContext, narrowContext, context,

    -- ** Utility functions
    localFunctions, importedFunctions, functionsFromName, functionsFromModule,
) where

import Data.List.Extra  (nubOrd)
import Data.Map         as Map

import Language.Qux.Syntax


-- | Global context that holds function definition types.
data Context = Context {
    module_     :: [Id],                    -- ^ The current module identifier.
    imports     :: [[Id]],                  -- ^ The imports required by the module.
    functions   :: Map [Id] [(Type, Id)]    -- ^ A map of qualified identifiers to function types
                                            --   (including parameter names).
    }
    deriving (Eq, Show)


-- | An empty context.
emptyContext :: Context
emptyContext = Context { module_ = [], imports = [], functions = Map.empty }

-- | Returns a base context for the given programs.
--   The base context populates @functions@ but not @module_@.
baseContext :: [Program] -> Context
baseContext programs = Context {
    module_     = [],
    imports     = [],
    functions   = Map.fromList [(module_ ++ [name], type_) |
        (Program module_ decls) <- programs,
        (FunctionDecl _ name type_ _) <- decls
        ]
    }

-- | Narrows down the given context and makes it specific to the given program.
--   This method removes any function references that aren't imported by the program.
narrowContext :: Context -> Program -> Context
narrowContext context (Program module_' decls) = context {
    module_     = module_',
    imports     = imports',
    functions   = Map.filterWithKey (\id _ -> init id `elem` module_':imports') (functions context)
    }
    where
        imports' = nubOrd [id | (ImportDecl id) <- decls]

-- | Returns a specific context for the given program.
--   @context programs program = narrowContext (baseContext programs) program@.
context :: [Program] -> Program -> Context
context programs program = narrowContext (baseContext programs) program


-- | Filters the local functions from the context.
--   This is functions defined (or declared) in the current module.
localFunctions :: Context -> Map [Id] [(Type, Id)]
localFunctions context = Map.filterWithKey (\id _ -> init id == module_') (functions context)
    where
        module_' = module_ context

-- | Filters the imported functions from the context.
importedFunctions :: Context -> Map [Id] [(Type, Id)]
importedFunctions context = Map.filterWithKey (\id _ -> init id /= module_') (functions context)
    where
        module_' = module_ context

-- | Filters the functions that export the given function name.
functionsFromName :: Context -> Id -> Map [Id] [(Type, Id)]
functionsFromName context name = Map.filterWithKey (\id _ -> last id == name) (functions context)

-- | Filters the functions that are exported by the given module.
functionsFromModule :: Context -> [Id] -> Map [Id] [(Type, Id)]
functionsFromModule context id = Map.filterWithKey (\id' _ -> init id' == id) (functions context)

