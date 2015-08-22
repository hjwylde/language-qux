
{-|
Module      : Language.Qux.Lexer
Description : A Parsec lexer for the Qux language.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

A "Text.Parsec" lexer for the Qux language.
-}

module Language.Qux.Lexer (
    -- * Lexer
    lexer,

    -- ** Language elements
    identifier, natural, operator, reserved, symbol, whiteSpace,

    -- ** Symbols
    brackets, colon, comma, parens, rightArrow
) where

import Control.Monad.State

import Data.List

import Text.Parsec hiding (State)
import Text.Parsec.Char
import qualified Text.Parsec.Token as Token


-- | Lexer for the Qux language definition.
lexer = Token.makeTokenParser quxDef


-- | Lexumes an identifier matching @[a-zA-Z_][a-zA-Z_']*@.
identifier = Token.identifier lexer

-- | Lexumes a natural number (decimal, octal or hex).
natural = Token.natural lexer

-- | Lexumes a reserved operator from 'operators'.
operator = Token.reservedOp lexer

-- | Lexumes a reserved keyword from 'keywords'.
reserved = Token.reserved lexer

-- | Lexumes a symbol.
symbol = Token.symbol lexer

-- |    Lexumes white space.
--      White space includes comments.
whiteSpace = Token.whiteSpace lexer


-- | @brackets p@ lexumes @p@ surrounded by @[..]@.
brackets = Token.brackets lexer

-- | Lexumes a colon, @:@.
colon = Token.colon lexer

-- | Lexumes a comma, @,@.
comma = Token.comma lexer

-- | @parens p@ lexumes @p@ surrounded by @(..)@.
parens = Token.parens lexer

-- | Lexumes a right arrow, @->@.
rightArrow = symbol "->"


quxDef :: Token.GenLanguageDef String st (State SourcePos)
quxDef = Token.LanguageDef
    commentStart
    commentEnd
    commentLine
    nestedComments
    identStart
    identLetter
    opStart
    opLetter
    reservedNames
    reservedOpNames
    caseSensitive
        where
            commentStart    = "/*"
            commentEnd      = "*/"
            commentLine     = "#"
            nestedComments  = False
            identStart      = letter <|> char '_'
            identLetter     = alphaNum <|> oneOf ['_', '\'']
            opStart         = oneOf []
            opLetter        = oneOf []
            reservedNames   = keywords
            reservedOpNames = operators
            caseSensitive   = True

keywords = [
    "_",
    "else",
    "false",
    "if",
    "nil",
    "return",
    "true",
    "while",
    "Bool",
    "Int",
    "Nil"
    ]

operators = [
    "!!",
    "*", "/", "%",
    "+", "-",
    "<", "<=", ">", ">=",
    "==", "!="
    ]

