
{-|
Module      : Language.Qux.Annotated.Exception
Description : Exceptions and utility raising functions.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Exceptions and utility functions.
-}

module Language.Qux.Annotated.Exception (
    -- * Type exception
    TypeException(..),
    pos, message,
) where

import Data.List (intercalate)

import Language.Qux.Annotated.Parser (SourcePos)
import Language.Qux.Syntax


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
                    | MismatchedType SourcePos Id [Id]      -- ^ Indicates a type mismatch.
    deriving Eq

instance Show TypeException where
    show e = show (pos e) ++ ":\n" ++ message e

-- | Extracts the source position from the exception.
pos :: TypeException -> SourcePos
pos (TypeException p _)             = p
pos (DuplicateFunctionName p _)     = p
pos (DuplicateParameterName p _)    = p
pos (InvalidFunctionCall p _ _)     = p
pos (MismatchedType p _ _)          = p

-- | Creates a human understandable message from the exception.
message :: TypeException -> String
message (TypeException _ m)                         = m
message (DuplicateFunctionName _ name)              = "duplicate function name \"" ++ name ++ "\""
message (DuplicateParameterName _ name)             = "duplicate parameter name \"" ++ name ++ "\""
message (InvalidFunctionCall _ received expected)   = intercalate " " [
    "invalid arguments count", show received,
    "\nexpecting", show expected
    ]
message (MismatchedType _ received expects)         = intercalate " " [
    "unexpected type", "\"" ++ received ++ "\"",
    "\nexpecting", sentence "or" expects
    ]
    where
        sentence _ [x]  = x
        sentence sep xs = intercalate " " [intercalate ", " (map show $ init xs), sep, show $ last xs]

