
{-|
Module      : Language.Qux.Lexer
Description : A Parsec lexer for the Qux language.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

A "Text.Parsec" lexer for the Qux language.
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Language.Qux.Lexer where

import Control.Monad.State

import              Text.Parsec         hiding (State)
import qualified    Text.Parsec.Token   as Token


lexer :: Token.GenTokenParser String u (State SourcePos)
lexer = Token.makeTokenParser quxDef


identifier :: ParsecT String u (State SourcePos) String
identifier = Token.identifier lexer

natural :: ParsecT String u (State SourcePos) Integer
natural = Token.natural lexer

operator :: String -> ParsecT String u (State SourcePos) ()
operator = Token.reservedOp lexer

reserved :: String -> ParsecT String u (State SourcePos) ()
reserved = Token.reserved lexer

symbol :: String -> ParsecT String u (State SourcePos) String
symbol = Token.symbol lexer

symbol_ :: String -> ParsecT String u (State SourcePos) ()
symbol_ = void . symbol

whiteSpace :: ParsecT String u (State SourcePos) ()
whiteSpace = Token.whiteSpace lexer


brackets :: ParsecT String u (State SourcePos) a -> ParsecT String u (State SourcePos) a
brackets = Token.brackets lexer

colon :: ParsecT String u (State SourcePos) ()
colon = symbol_ ":"

comma :: ParsecT String u (State SourcePos) ()
comma = symbol_ ","

dot :: ParsecT String u (State SourcePos) ()
dot = symbol_ "."

parens :: ParsecT String u (State SourcePos) a -> ParsecT String u (State SourcePos) a
parens = Token.parens lexer

pipes :: ParsecT String u (State SourcePos) a -> ParsecT String u (State SourcePos) a
pipes p = Token.lexeme lexer $ between (symbol "|") (symbol "|") p

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
    "else", "if", "import", "module", "return", "while",
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

