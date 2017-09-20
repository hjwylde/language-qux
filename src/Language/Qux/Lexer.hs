{-|
Module      : Language.Qux.Lexer
Description : A Parsec lexer for the Qux language.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

A "Text.Parsec" lexer for the Qux language.
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Language.Qux.Lexer where

import Control.Monad

import           Text.Parsec
import qualified Text.Parsec.Token as Token

lexer :: Monad m => Token.GenTokenParser String u m
lexer = Token.makeTokenParser quxDef

charLiteral :: Monad m => ParsecT String u m Char
charLiteral = Token.charLiteral lexer

identifier :: Monad m => ParsecT String u m String
identifier = lookAhead (lower <|> char '_') *> Token.identifier lexer

typeIdentifier :: Monad m => ParsecT String u m String
typeIdentifier = lookAhead upper *> Token.identifier lexer

natural :: Monad m => ParsecT String u m Integer
natural = Token.natural lexer

operator :: Monad m => String -> ParsecT String u m ()
operator = Token.reservedOp lexer

reserved :: Monad m => String -> ParsecT String u m ()
reserved = Token.reserved lexer

symbol :: Monad m => String -> ParsecT String u m String
symbol = Token.symbol lexer

symbol_ :: Monad m => String -> ParsecT String u m ()
symbol_ = void . symbol

whiteSpace :: Monad m => ParsecT String u m ()
whiteSpace = Token.whiteSpace lexer

colon :: Monad m => ParsecT String u m ()
colon = symbol_ ":"

comma :: Monad m => ParsecT String u m ()
comma = symbol_ ","

dot :: Monad m => ParsecT String u m ()
dot = symbol_ "."

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens = Token.parens lexer

rightArrow :: Monad m => ParsecT String u m ()
rightArrow = symbol_ "->"

quxDef :: Monad m => Token.GenLanguageDef String u m
quxDef = Token.LanguageDef commentStart commentEnd commentLine nestedComments identStart identLetter
    opStart opLetter reservedNames reservedOpNames caseSensitive
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

keywords :: [String]
keywords =
    [ "_"
    , "external", "import", "module", "type"
    , "else", "if", "return", "while"
    , "false", "nil", "true"
    , "Any", "Bool", "Int", "Nil"
    ]

operators :: [String]
operators =
    [ "*", "/", "%"
    , "+", "-"
    , "<", "<=", ">", ">="
    , "==", "!="
    ]
