
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
    identifier, natural, operator, reserved, symbol, symbol_, whiteSpace,

    -- ** Symbols
    brackets, colon, comma, parens, pipes, rightArrow
) where

import Control.Monad.State

import              Text.Parsec         hiding (State)
import qualified    Text.Parsec.Token   as Token


-- | Lexer for the Qux language definition.
lexer :: Token.GenTokenParser String u (State SourcePos)
lexer = Token.makeTokenParser quxDef


-- | Lexemes an identifier matching @[a-zA-Z_][a-zA-Z_']*@.
identifier :: ParsecT String u (State SourcePos) String
identifier = Token.identifier lexer

-- | Lexemes a natural number (decimal, octal or hex).
natural :: ParsecT String u (State SourcePos) Integer
natural = Token.natural lexer

-- | Lexemes a reserved operator from 'operators'.
operator :: String -> ParsecT String u (State SourcePos) ()
operator = Token.reservedOp lexer

-- | Lexemes a reserved keyword from 'keywords'.
reserved :: String -> ParsecT String u (State SourcePos) ()
reserved = Token.reserved lexer

-- | Lexemes a symbol, returning the same string.
symbol :: String -> ParsecT String u (State SourcePos) String
symbol = Token.symbol lexer

-- | Lexemes a symbol, ignoring the return result.
symbol_ :: String -> ParsecT String u (State SourcePos) ()
symbol_ = void . symbol

-- |    Lexemes white space.
--      White space includes comments.
whiteSpace :: ParsecT String u (State SourcePos) ()
whiteSpace = Token.whiteSpace lexer


-- | @brackets p@ lexemes @p@ surrounded by @[..]@.
brackets :: ParsecT String u (State SourcePos) a -> ParsecT String u (State SourcePos) a
brackets = Token.brackets lexer

-- | Lexemes a colon, @:@.
colon :: ParsecT String u (State SourcePos) ()
colon = symbol_ ":"

-- | Lexemes a comma, @,@.
comma :: ParsecT String u (State SourcePos) ()
comma = symbol_ ","

-- | @parens p@ lexemes @p@ surrounded by @(..)@.
parens :: ParsecT String u (State SourcePos) a -> ParsecT String u (State SourcePos) a
parens = Token.parens lexer

-- | @pipes p@ lexemes @p@ surrounded by @|..|@.
pipes :: ParsecT String u (State SourcePos) a -> ParsecT String u (State SourcePos) a
pipes p = Token.lexeme lexer $ between (symbol "|") (symbol "|") p

-- | Lexemes a right arrow, @->@.
rightArrow :: ParsecT String u (State SourcePos) ()
rightArrow = symbol_ "->"


quxDef :: Token.GenLanguageDef String u (State SourcePos)
quxDef = Token.LanguageDef commentStart commentEnd commentLine nestedComments identStart identLetter
    opStart opLetter reservedNames reservedOpNames caseSensitive
        where
            commentStart    = "/*"
            commentEnd      = "*/"
            commentLine     = "#"
            nestedComments  = False
            identStart      = lower <|> char '_'
            identLetter     = alphaNum <|> oneOf ['_', '\'']
            opStart         = oneOf []
            opLetter        = oneOf []
            reservedNames   = keywords
            reservedOpNames = operators
            caseSensitive   = True

keywords :: [String]
keywords = [
    "_",
    "else", "if", "return", "while",
    "false", "nil", "true",
    "Bool", "Int", "Nil"
    ]

operators :: [String]
operators = [
    "!!", "|",
    "*", "/", "%",
    "+", "-",
    "<", "<=", ">", ">=",
    "==", "!="
    ]

