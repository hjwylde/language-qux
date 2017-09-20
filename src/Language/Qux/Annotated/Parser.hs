{-|
Module      : Language.Qux.Annotated.Parser
Description : A Parsec indentation-based parser for generating a program.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : hjwylde@gmail.com

A "Text.Parsec" indentation-based parser for generating a 'Program'.
-}

module Language.Qux.Annotated.Parser (
    -- * Parser type
    Parser, ParseError,
    parse,

    -- * Source position type
    SourcePos,
    sourceName, sourceLine, sourceColumn,

    -- * Parsers
    id_, typeId, program, decl, attribute, stmt, expr, value, type_,
) where

import Control.Monad.Trans.Except

import Data.Functor.Identity

import Language.Qux.Annotated.Syntax
import Language.Qux.Lexer

import Text.Parsec        hiding (State, parse)
import Text.Parsec.Expr
import Text.Parsec.Indent

-- | An 'IndentParser'.
type Parser a = IndentParser String () a

-- | @parse parser sourceName input@ parses @input@ using @parser@.
--   Returns either a 'ParseError' or @a@.
--   This method wraps 'runIndentParser' by turning the 'Either' into an 'Except'.
parse :: Parser a -> SourceName -> String -> Except ParseError a
parse parser sourceName input = except $ runIndentParser parser () sourceName input

-- | 'Id' parser (must start with a lowercase letter or underscore).
id_ :: Parser (Id SourcePos)
id_ = Id <$> getPosition <*> identifier <?> "identifier"

-- | 'Id' parser for types (must start with an uppercase letter).
typeId :: Parser (Id SourcePos)
typeId = Id <$> getPosition <*> typeIdentifier <?> "type identifier"

-- | 'Program' parser.
program :: Parser (Program SourcePos)
program = do
    pos <- getPosition

    whiteSpace
    checkIndent
    reserved "module"
    module_ <- (same >> id_) `sepBy1` dot
    checkIndent
    decls <- block decl
    eof
    return $ Program pos module_ decls
    <?> "program"

-- | 'Decl' parser.
decl :: Parser (Decl SourcePos)
decl = choice [functionOrTypeDecl, importDecl] <?> "declaration"
    where
        functionOrTypeDecl = do
            pos <- getPosition

            attrs <- many attribute

            choice
                [ if External undefined `elem` attrs
                  then externalFunctionDecl pos attrs
                  else functionDecl pos attrs
                , typeDecl pos attrs
                ] <?> "function or type declaration"

        externalFunctionDecl pos attrs = do
            name <- id_
            symbol_ "::"
            parameterTypes <- withPos $ try (fmap (,) type_ <+/> return (Id pos "_")) `endBy` rightArrow
            returnType <- type_

            return $ FunctionDecl pos attrs name (parameterTypes ++ [(returnType, Id pos "@")]) []

        functionDecl pos attrs = do
            name <- id_
            symbol_ "::"
            parameterTypes <- withPos $ try (fmap (,) type_ <+/> id_) `endBy` rightArrow
            returnType <- type_
            stmts <- colon >> indented >> block stmt

            return $ FunctionDecl pos attrs name (parameterTypes ++ [(returnType, Id pos "@")]) stmts

        typeDecl pos attrs = do
            reserved "type"
            name <- typeId

            return $ TypeDecl pos attrs name

        importDecl = do
            pos <- getPosition

            reserved "import"
            id <- id_ `sepBy1` dot

            return $ ImportDecl pos id

-- | 'Attribute' parser.
attribute :: Parser (Attribute SourcePos)
attribute = getPosition >>= \pos -> External pos <$ reserved "external"

-- | 'Stmt' parser.
stmt :: Parser (Stmt SourcePos)
stmt = choice [ifStmt, returnStmt, whileStmt] <?> "statement"
    where
        ifStmt      = do
            pos <- getPosition

            reserved "if"
            condition <- expr
            colon
            indented
            trueStmts <- block stmt
            falseStmts <- option [] (checkIndent >> withBlock' (do { reserved "else"; colon }) stmt)

            return $ IfStmt pos condition trueStmts falseStmts
        returnStmt  = ReturnStmt <$> getPosition <* reserved "return" <*> expr
        whileStmt   = do
            pos <- getPosition

            withBlock (WhileStmt pos) (reserved "while" *> expr <* colon) stmt

-- | 'Expr' parser.
expr :: Parser (Expr SourcePos)
expr = buildExpressionParser table (try application <|> term) <?> "expression"

application :: Parser (Expr SourcePos)
application = ApplicationExpr <$> getPosition <*> id_ <*> many (sameOrIndented >> term)

term :: Parser (Expr SourcePos)
term = getPosition >>= \pos -> choice
    [ parens expr
    , ApplicationExpr pos <$> id_ <*> return []
    , ValueExpr pos       <$> value
    ]

table :: OperatorTable String () (IndentT Identity) (Expr SourcePos)
table =
    [ [ Prefix (unaryExpr Neg "-")
      ],
      [ Infix (binaryExpr Mul "*") AssocLeft
      , Infix (binaryExpr Div "/") AssocLeft
      , Infix (binaryExpr Mod "%") AssocLeft
      ],
      [ Infix (binaryExpr Add "+") AssocLeft
      , Infix (binaryExpr Sub "-") AssocLeft
      ],
      [ Infix (binaryExpr Lte "<=") AssocLeft
      , Infix (binaryExpr Lt "<") AssocLeft
      , Infix (binaryExpr Gte ">=") AssocLeft
      , Infix (binaryExpr Gt ">") AssocLeft
      ],
      [ Infix (binaryExpr Eq "==") AssocLeft
      , Infix (binaryExpr Neq "!=") AssocLeft
      ]
    ]

binaryExpr :: BinaryOp -> String -> Parser (Expr SourcePos -> Expr SourcePos -> Expr SourcePos)
binaryExpr op sym = getPosition >>= \pos -> BinaryExpr pos op <$ operator sym

unaryExpr :: UnaryOp -> String -> Parser (Expr SourcePos -> Expr SourcePos)
unaryExpr op sym = getPosition >>= \pos -> UnaryExpr pos op <$ operator sym

-- | 'Value' parser.
--   A value doesn't have a source position attached as this can be retrieved from a 'ValueExpr'.
value :: Parser Value
value = choice
    [ BoolValue False <$  reserved "false"
    , BoolValue True  <$  reserved "true"
    , CharValue       <$> charLiteral
    , IntValue        <$> natural
    , NilValue        <$  reserved "nil"
    ] <?> "value"

-- | 'Type' parser.
type_ :: Parser (Type SourcePos)
type_ = getPosition >>= \pos -> choice
    [ AnyType pos <$  reserved "Any"
    , BoolType pos <$ reserved "Bool"
    , CharType pos <$ reserved "Char"
    , IntType  pos <$ reserved "Int"
    , NilType  pos <$ reserved "Nil"
    ] <?> "type"
