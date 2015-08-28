
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
    brackets, colon, comma, parens, pipes, rightArrow
) where

import Control.Monad.State

import Text.Parsec hiding (State)
import qualified Text.Parsec.Token as Token


-- | Lexer for the Qux language definition.
lexer = Token.makeTokenParser quxDef


-- | Lexemes an identifier matching @[a-zA-Z_][a-zA-Z_']*@.
identifier = Token.identifier lexer

-- | Lexemes a natural number (decimal, octal or hex).
natural = Token.natural lexer

-- | Lexemes a reserved operator from 'operators'.
operator = Token.reservedOp lexer

-- | Lexemes a reserved keyword from 'keywords'.
reserved = Token.reserved lexer

-- | Lexemes a symbol.
symbol = Token.symbol lexer

-- |    Lexemes white space.
--      White space includes comments.
whiteSpace = Token.whiteSpace lexer


-- | @brackets p@ lexemes @p@ surrounded by @[..]@.
brackets = Token.brackets lexer

-- | Lexemes a colon, @:@.
colon = Token.colon lexer

-- | Lexemes a comma, @,@.
comma = Token.comma lexer

-- | @parens p@ lexemes @p@ surrounded by @(..)@.
parens = Token.parens lexer

-- | @pipes p@ lexemes @p@ surrounded by @|..|@.
pipes p = Token.lexeme lexer $ between (symbol "|") (symbol "|") p

-- | Lexemes a right arrow, @->@.
rightArrow = symbol "->"


quxDef :: Token.GenLanguageDef String u (State SourcePos)
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
    "else", "if", "return", "while",
    "false", "nil", "true",
    "Bool", "Int", "Nil"
    ]

operators = [
    "!!", "|",
    "*", "/", "%",
    "+", "-",
    "<", "<=", ">", ">=",
    "==", "!="
    ]

