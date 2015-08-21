
module Language.Qux.Exception (
    TypeException,
    duplicateFunctionName, duplicateParameterName, invalidArgumentsCount, mismatchedType
) where

import Data.List

import Language.Qux.Ast
import Language.Qux.PrettyPrinter

import Text.PrettyPrint


newtype TypeException = TypeException String

instance Show TypeException where
    show (TypeException message) = message


duplicateFunctionName :: String -> TypeException
duplicateFunctionName name = TypeException $ "duplicate function name '" ++ name ++ "'"

duplicateParameterName :: String -> TypeException
duplicateParameterName name = TypeException $ "duplicate parameter name '" ++ name ++ "'"

invalidArgumentsCount :: Int -> Int -> TypeException
invalidArgumentsCount expected received = TypeException $ intercalate " " [
    "invalid arguments count",
    show received,
    "\nexpecting",
    show expected
    ]

mismatchedType :: [Type] -> Type -> TypeException
mismatchedType expects received = TypeException $ intercalate " " [
    "unexpected type",
    render $ quotes (typeDoc received),
    "\nexpecting",
    sentence "or" (map (render . typeDoc) expects)
    ]

sentence :: String -> [String] -> String
sentence _ []   = ""
sentence _ [x]  = x
sentence sep xs = intercalate " " [
    intercalate ", " (map show $ init xs),
    sep,
    show $ last xs
    ]

