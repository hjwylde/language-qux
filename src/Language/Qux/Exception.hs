
{-|
Module      : Language.Qux.Exception
Description : Exceptions and utility raising functions.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Exceptions and utility creation functions.
-}

module Language.Qux.Exception (
    -- * Type exceptions
    TypeException,

    -- ** Creation functions
    duplicateFunctionName, duplicateParameterName, invalidArgumentsCount, mismatchedType
) where

import Data.List

import Language.Qux.PrettyPrinter
import Language.Qux.Syntax

import Text.PrettyPrint


-- | An exception that occurs during type checking. See "Language.Qux.TypeChecker".
newtype TypeException = TypeException String

instance Show TypeException where
    show (TypeException message) = message


-- |    @duplciateFunctionName name@ creates a 'TypeException' indicating that a duplicate function
--      declaration for @name@ was found.
duplicateFunctionName :: String -> TypeException
duplicateFunctionName name = TypeException $ "duplicate function name '" ++ name ++ "'"

-- |    @duplicateParameterName name@ creates a 'TypeException' indicating that a duplicate
--      parameter @name@ was found.
duplicateParameterName :: String -> TypeException
duplicateParameterName name = TypeException $ "duplicate parameter name '" ++ name ++ "'"

-- |    @invalidArgumentsCount expected received@ creates a 'TypeException' indicating that an
--      invalid number of arguments (@received@) was passed to a function expecting @expected@.
invalidArgumentsCount :: Int -> Int -> TypeException
invalidArgumentsCount expected received = TypeException $ intercalate " " [
    "invalid arguments count",
    show received,
    "\nexpecting",
    show expected
    ]

-- |    @mismatchedType expects received@ creates a 'TypeException' indicating that one of @expects@
--      was expected.
mismatchedType :: [Type] -> Type -> TypeException
mismatchedType expects received = TypeException $ intercalate " " [
    "unexpected type",
    prettyOneLine $ quotes (pPrint received),
    "\nexpecting",
    sentence "or" (map prettyOneLine expects)
    ]

sentence :: String -> [String] -> String
sentence _ []   = ""
sentence _ [x]  = x
sentence sep xs = intercalate " " [
    intercalate ", " (map show $ init xs),
    sep,
    show $ last xs
    ]

