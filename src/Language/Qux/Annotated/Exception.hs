
{-|
Module      : Language.Qux.Annotated.Exception
Description : Exceptions and utility raising functions.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Exceptions and utility functions.
-}

module Language.Qux.Annotated.Exception (
    -- * Type class
    CompilerException(..),

    -- * Type exception
    TypeException(..),

    -- * Resolve exception
    ResolveException(..),
) where

import Control.Exception

import Data.List        (intercalate)
import Data.Typeable

import Language.Qux.Annotated.Parser (SourcePos)
import Language.Qux.Syntax

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass


class CompilerException e where
    -- | Extracts the source position from the exception.
    pos :: e -> SourcePos

    -- | Creates a human understandable message from the exception.
    message :: e -> String


-- | An exception that occurs during type checking. See "Language.Qux.Annotated.TypeChecker".
data TypeException  = TypeException SourcePos String        -- ^ A generic type exception with a
                                                            --   position and message.
                    | DuplicateFunctionName SourcePos Id    -- ^ Indicates a function of the given
                                                            --   name already exists.
                    | DuplicateParameterName SourcePos Id   -- ^ Indicates a parameter of the given
                                                            --   name already exists.
                    | InvalidFunctionCall SourcePos Int Int -- ^ Indicates the wrong number of
                                                            --   arguments was passed to the
                                                            --   function call.
                    | MismatchedType SourcePos Type [Type]  -- ^ Indicates a type mismatch.
    deriving (Eq, Typeable)

instance CompilerException TypeException where
    pos (TypeException p _)             = p
    pos (DuplicateFunctionName p _)     = p
    pos (DuplicateParameterName p _)    = p
    pos (InvalidFunctionCall p _ _)     = p
    pos (MismatchedType p _ _)          = p

    message (TypeException _ m)                         = m
    message (DuplicateFunctionName _ name)              = "duplicate function name \"" ++ name ++ "\""
    message (DuplicateParameterName _ name)             = "duplicate parameter name \"" ++ name ++ "\""
    message (InvalidFunctionCall _ received expected)   = intercalate " " [
        "invalid arguments count", show received,
        "\nexpecting", show expected
        ]
    message (MismatchedType _ received expects)         = intercalate " " [
        "unexpected type", "\"" ++ (renderOneLine $ pPrint received) ++ "\"",
        "\nexpecting", sentence "or" (map (renderOneLine . pPrint) expects)
        ]

instance Exception TypeException

instance Show TypeException where
    show e = show (pos e) ++ ":\n" ++ message e


-- | An exception that occurs during name resolution. See "Language.Qux.Annotated.NameResolver".
data ResolveException   = ResolveException SourcePos String         -- ^ A generic type exception with a
                                                                    --   position and message.
                        | AmbiguousFunctionCall SourcePos Id [[Id]] -- ^ Indicates multiple exporters of a function.
                        | DuplicateImport SourcePos [Id]            -- ^ Indicates duplicate import found.
                        | ImportNotFound SourcePos [Id]             -- ^ Indicates import not found.
                        | InvalidVariableAccess SourcePos Id        -- ^ Indicates arguments passed on local variable access.
                        | UndefinedFunctionCall SourcePos Id        -- ^ Indicates function not found.
    deriving (Eq, Typeable)

instance CompilerException ResolveException where
    pos (ResolveException p _)          = p
    pos (AmbiguousFunctionCall p _ _)   = p
    pos (DuplicateImport p _)           = p
    pos (ImportNotFound p _)            = p
    pos (InvalidVariableAccess p _)     = p
    pos (UndefinedFunctionCall p _)     = p

    message (ResolveException _ m)                      = m
    message (AmbiguousFunctionCall _ name exporters)    = intercalate " " [
        "ambiguous call to function", "\"" ++ name ++ "\"",
        "\nexported from", sentence "and" (map (intercalate ".") exporters)
        ]
    message (DuplicateImport _ id)                      = "duplicate import \"" ++ intercalate "." id ++ "\""
    message (ImportNotFound _ id)                       = "cannot find module \"" ++ intercalate "." id ++ "\""
    message (InvalidVariableAccess _ name)              = "arguments passed when accessing local variable \"" ++ name ++ "\""
    message (UndefinedFunctionCall _ name)              = "call to undefined function \"" ++ name ++ "\""

instance Exception ResolveException

instance Show ResolveException where
    show e = show (pos e) ++ ":\n" ++ message e


-- Helper methods

renderOneLine :: Doc -> String
renderOneLine = renderStyle (style { mode = OneLineMode })

sentence :: String -> [String] -> String
sentence _ [x]  = x
sentence sep xs = intercalate " " [intercalate ", " (map show $ init xs), sep, show $ last xs]

