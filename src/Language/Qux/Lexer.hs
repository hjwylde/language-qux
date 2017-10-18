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
    [ "_", "()"
    , "external", "import", "module", "type"
    , "else", "if", "return", "while"
    , "false", "true"
    , "Any", "Bool", "Int", "Str"
    ]

operators :: [String]
operators =
    [ "*", "/", "%"
    , "+", "-"
    , "<", "<=", ">", ">="
    , "==", "!="
    ]

identifier :: Monad m => ParsecT String u m String
identifier = lookAhead (lower <|> char '_') *> Token.identifier lexer

typeIdentifier :: Monad m => ParsecT String u m String
typeIdentifier = lookAhead upper *> Token.identifier lexer

string :: Monad m => ParsecT String u m String
string = Token.stringLiteral lexer

natural :: Monad m => ParsecT String u m Integer
natural = Token.natural lexer

operator :: Monad m => String -> ParsecT String u m ()
operator = Token.reservedOp lexer

keyword :: Monad m => String -> ParsecT String u m ()
keyword = Token.reserved lexer

separator :: Monad m => String -> ParsecT String u m String
separator = Token.symbol lexer

separator_ :: Monad m => String -> ParsecT String u m ()
separator_ = void . separator

colon :: Monad m => ParsecT String u m ()
colon = separator_ ":"

comma :: Monad m => ParsecT String u m ()
comma = separator_ ","

dcolon :: Monad m => ParsecT String u m ()
dcolon = separator_ "::"

dot :: Monad m => ParsecT String u m ()
dot = separator_ "."

parens :: Monad m => ParsecT String u m a -> ParsecT String u m a
parens = Token.parens lexer

rarrow :: Monad m => ParsecT String u m ()
rarrow = separator_ "->"

whiteSpace :: Monad m => ParsecT String u m ()
whiteSpace = Token.whiteSpace lexer
