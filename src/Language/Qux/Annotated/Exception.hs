
{-|
Module      : Language.Qux.Annotated.Exception
Description : Exceptions and utility raising functions.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Exceptions and utility creation functions.
-}

module Language.Qux.Annotated.Exception (
    -- * Type exception
    TypeException,
    pos, message,

    -- ** Creation functions
    duplicateFunctionName, duplicateParameterName, invalidArgumentsCount, mismatchedType,
    undefinedFunctionCall
) where

import Data.List (intercalate)

import Language.Qux.Annotated.Parser (SourcePos)
import Language.Qux.Annotated.PrettyPrinter
import qualified Language.Qux.Annotated.Syntax as Ann
import Language.Qux.Syntax

import Text.PrettyPrint (doubleQuotes)


-- | An exception that occurs during type checking. See "Language.Qux.Annotated.TypeChecker".
data TypeException = TypeException SourcePos String

instance Show TypeException where
    show (TypeException pos message) = show pos ++ ":\n" ++ message

-- | Extracts the source position from the exception.
pos :: TypeException -> SourcePos
pos (TypeException p _) = p

-- | Extracts the message from the exception.
message :: TypeException -> String
message (TypeException _ m) = m


-- |    @duplciateFunctionName decl@ creates a 'TypeException' indicating that a duplicate function
--      declaration @decl@ was found.
duplicateFunctionName :: Ann.Decl SourcePos -> TypeException
duplicateFunctionName (Ann.FunctionDecl pos (Ann.Id _ name) _ _) = TypeException pos ("duplicate function name \"" ++ name ++ "\"")

-- |    @duplicateParameterName parameter@ creates a 'TypeException' indicating that a duplicate
--      parameter @parameter@ was found.
duplicateParameterName :: Ann.Id SourcePos -> TypeException
duplicateParameterName (Ann.Id pos name) = TypeException pos ("duplicate parameter name \"" ++ name ++ "\"")
--
-- |    @invalidArgumentsCount received expected@ creates a 'TypeException' indicating that an
--      application call (@received@) with an invalid number of arguments was made to a function
--      expecting @expected@.
invalidArgumentsCount :: Ann.Expr SourcePos -> Int -> TypeException
invalidArgumentsCount (Ann.ApplicationExpr pos _ arguments) expected = TypeException pos $ intercalate " " [
    "invalid arguments count", show $ length arguments,
    "\nexpecting", show expected
    ]
invalidArgumentsCount _ _ = undefined

-- |    @mismatchedType received expects@ creates a 'TypeException' indicating that one of @expects@
--      was expected.
mismatchedType :: Ann.Type SourcePos -> [Type] -> TypeException
mismatchedType received expects = TypeException (Ann.ann received) $ intercalate " " [
    "unexpected type", renderOneLine $ doubleQuotes (pPrint received),
    "\nexpecting", sentence "or" (map (renderOneLine . doubleQuotes . pPrint) expects)
    ]

-- |    @undefinedFunctionCall app@ creates a 'TypeException' indicating that an application call
--      (@app@) was made to an undefined function.
undefinedFunctionCall :: Ann.Expr SourcePos -> TypeException
undefinedFunctionCall (Ann.ApplicationExpr pos (Ann.Id _ name) _) = TypeException pos ("call to undefined function \"" ++ name ++ "\"")
undefinedFunctionCall _ = undefined


sentence :: String -> [String] -> String
sentence _ [x]  = x
sentence sep xs = intercalate " " [intercalate ", " (map show $ init xs), sep, show $ last xs]

