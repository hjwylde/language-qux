{-|
Module      : Language.Qux.Context
Description : Context data type and utility methods.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

Context data type and utility methods.
-}

{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Qux.Context (
    -- * Context
    Context(..),
    module_, imports, functions, types,

    -- ** Creating contexts
    emptyContext, baseContext, narrowContext, context,

    -- ** Getters
    local, imported, withName,
) where

import Control.Lens hiding (Context)

import           Data.List.Extra
import           Data.Map        (Map)
import qualified Data.Map        as Map

import Language.Qux.Syntax

-- | Global context that holds function definition types.
data Context = Context
    { _module_   :: [Id]                    -- ^ The current module identifier.
    , _imports   :: [[Id]]                  -- ^ The imports required by the module.
    , _functions :: Map [Id] [(Type, Id)]   -- ^ A map of qualified identifiers to function types
                                            --   (including parameter names).
    , _types     :: [[Id]]                  -- ^ A list of qualified types.
    }
    deriving (Eq, Show)

makeLenses ''Context

-- | An empty context.
emptyContext :: Context
emptyContext = Context
    { _module_   = []
    , _imports   = []
    , _functions = Map.empty
    , _types     = []
    }

-- | Returns a base context for the given programs.
--   The base context populates @functions@ but not @module_@.
baseContext :: [Program] -> Context
baseContext programs = Context
    { _module_   = []
    , _imports   = []
    , _functions = Map.fromList
        [ (module_ ++ [name], type_)
        | (Program module_ decls) <- programs
        , (FunctionDecl _ name type_ _) <- decls
        ]
    , _types     =
        [ module_ ++ [name]
        | (Program module_ decls) <- programs
        , (TypeDecl _ name) <- decls
        ]
    }

-- | Narrows down the given context and makes it specific to the given program.
--   This method removes any function references that aren't imported by the program.
narrowContext :: Context -> Program -> Context
narrowContext context (Program module_' decls) = context
    & module_   .~ module_'
    & imports   .~ imports'
    & functions %~ Map.filterWithKey (\id _ -> init id `elem` module_':imports')
    & types     %~ filter (\id -> init id `elem` module_':imports')
    where
        imports' = nubOrd [id | (ImportDecl id) <- decls]

-- | Returns a specific context for the given program.
--   @context programs program = narrowContext (baseContext programs) program@.
context :: [Program] -> Program -> Context
context programs = narrowContext (baseContext programs)

-- | Filters the local functions and types from the context.
--   This is functions and types defined (or declared) in the current module.
local :: Getter Context Context
local = to $ \context -> context ^. withModule (context ^. module_)

-- | Filters the imported functions and types from the context.
--   This is functions and types not defined (or declared) in the current module.
imported :: Getter Context Context
imported = to $ \context -> context ^. withoutModule (context ^. module_)

-- | Filters the functions and types from the context.
--   This is functions and types defined (or declared) with the given name.
withName :: Id -> Getter Context Context
withName name = to $ \context -> context
    & functions %~ Map.filterWithKey (\id _ -> last id == name)
    & types %~ filter (\id -> last id == name)

-- | Filters the functions and types from the context.
--   This is functions and types defined (or declared) in the given module.
withModule :: [Id] -> Getter Context Context
withModule module_ = to $ \context -> context
    & functions %~ Map.filterWithKey (\id _ -> init id == module_)
    & types %~ filter (\id -> init id == module_)

-- | Filters the functions and types from the context.
--   This is functions and types not defined (or declared) in the given module.
withoutModule :: [Id] -> Getter Context Context
withoutModule module_ = to $ \context -> context
    & functions %~ Map.filterWithKey (\id _ -> init id /= module_)
    & types %~ filter (\id -> init id /= module_)
