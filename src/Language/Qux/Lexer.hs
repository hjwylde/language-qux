
module Language.Qux.Lexer (
    lexer,
    brackets, identifier, natural, operator, parens, reserved, symbol, whiteSpace,
    colon, comma, rightArrow
) where

import Control.Monad.State

import Data.List

import Text.Parsec hiding (State)
import Text.Parsec.Char
import qualified Text.Parsec.Token as Token


lexer = Token.makeTokenParser quxDef

brackets = Token.brackets lexer
identifier = Token.identifier lexer
natural = Token.natural lexer
operator = try . (Token.symbol lexer)
parens = Token.parens lexer
reserved = Token.reserved lexer
symbol = Token.symbol lexer
whiteSpace = Token.whiteSpace lexer

colon = Token.colon lexer
comma = Token.comma lexer
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
            opStart         = oneOf $ nub $ map head operators
            opLetter        = oneOf $ nub $ concatMap tail operators
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

operators = []

