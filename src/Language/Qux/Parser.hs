
{-|
Module      : Language.Qux.Parser
Description : A Parsec indentation-based parser for generating a program.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

A "Text.Parsec" indentation-based parser for generating a 'Program'.
-}

module Language.Qux.Parser (
    -- * Types
    Parser, ParseError,

    -- ** Parsing
    parse,

    -- ** Parsers
    program, decl, stmt, expr, value, type_
) where

import Control.Applicative
import Control.Monad.State

import Language.Qux.Ast
import Language.Qux.Lexer

import Text.Parsec hiding (State, (<|>), many, parse)
import Text.Parsec.Expr
import Text.Parsec.Indent


-- | A 'ParsecT' that retains indentation information.
type Parser a = ParsecT String () (State SourcePos) a


-- |    @parse parser sourceName input@ parses @input@ using @parser@.
--      Returns either a 'ParseError' or @a@.
--      This method wraps 'runParserT' by running the indentation resolver over the parser's state.
parse :: Parser a -> SourceName -> String -> Either ParseError a
parse parser sourceName input = runIndent sourceName $ runParserT parser () sourceName input


-- | 'Program' parser.
program :: Parser Program
program = do
    whiteSpace
    checkIndent
    decls <- block decl
    eof
    return $ Program decls

-- | 'Decl' parser.
decl :: Parser Decl
decl = do
    name <- identifier
    symbol "::"
    parameters <- (try $ (,) <$> type_ <*> identifier) `endBy` rightArrow
    returnType <- type_
    colon
    indented
    stmts <- block stmt
    return $ FunctionDecl name (parameters ++ [(returnType, "@")]) stmts
    <?> "function declaration"

-- | 'Stmt' parser.
stmt :: Parser Stmt
stmt = choice [
    ifStmt,
    ReturnStmt <$> (reserved "return" *> expr),
    withBlock WhileStmt (reserved "while" *> expr <* colon) stmt
    ] <?> "statement"
    where
        ifStmt = do
            reserved "if"
            condition <- expr
            colon
            indented
            trueStmts <- block stmt
            falseStmts <- option [] (checkIndent >> withBlock' (do { reserved "else"; colon }) stmt)
            return $ IfStmt condition trueStmts falseStmts

-- | 'Expr' parser.
expr :: Parser Expr
expr = buildExpressionParser table (try application <|> term) <?> "expression"

table :: OperatorTable String () (State SourcePos) Expr
table = [
    [
        Prefix (UnaryExpr Neg <$ operator "-")
    ],
    [
        Infix (BinaryExpr Acc <$ operator "!!") AssocLeft
    ],
    [
        Infix (BinaryExpr Mul <$ operator "*") AssocLeft,
        Infix (BinaryExpr Div <$ operator "/") AssocLeft,
        Infix (BinaryExpr Mod <$ operator "%") AssocLeft
    ],
    [
        Infix (BinaryExpr Add <$ operator "+") AssocLeft,
        Infix (BinaryExpr Sub <$ operator "-") AssocLeft
    ],
    [
        Infix (BinaryExpr Lte <$ operator "<=") AssocLeft,
        Infix (BinaryExpr Lt <$ operator "<") AssocLeft,
        Infix (BinaryExpr Gte <$ operator ">=") AssocLeft,
        Infix (BinaryExpr Gt <$ operator ">") AssocLeft
    ],
    [
        Infix (BinaryExpr Eq <$ operator "==") AssocLeft,
        Infix (BinaryExpr Neq <$ operator "!=") AssocLeft
    ]
    ]

application :: Parser Expr
application = ApplicationExpr <$> identifier <*> many (sameOrIndented >> term)

term :: Parser Expr
term = choice [
    flip ApplicationExpr [] <$> identifier,
    ListExpr <$> brackets (expr `sepEndBy` comma),
    parens expr,
    (UnaryExpr Len) <$> pipes expr,
    ValueExpr <$> value
    ] <?> "term"

-- | 'Value' parser.
value :: Parser Value
value = choice [
    BoolValue False <$ reserved "false",
    BoolValue True <$ reserved "true",
    IntValue <$> natural,
    NilValue <$ reserved "nil"
    ] <?> "value"

-- | 'Type' parser.
type_ :: Parser Type
type_ = choice [
    BoolType <$ reserved "Bool",
    IntType <$ reserved "Int",
    ListType <$> brackets type_,
    NilType <$ reserved "Nil"
    ] <?> "type"

