
module Language.Qux.Parser where

import Control.Applicative
import Control.Monad.State

import Language.Qux.Ast
import Language.Qux.Lexer

import Text.Parsec hiding (State, (<|>), many)
import Text.Parsec.Expr
import Text.Parsec.Indent hiding (IndentParser)


type IndentParser a = ParsecT String () (State SourcePos) a

indentParse :: IndentParser a -> SourceName -> String -> Either ParseError a
indentParse parser sourceName input = runIndent sourceName $ runParserT parser () sourceName input

program :: IndentParser Program
program = do
    whiteSpace
    checkIndent
    decls <- block decl
    eof
    return $ Program decls

decl :: IndentParser Decl
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

stmt :: IndentParser Stmt
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

expr :: IndentParser Expr
expr = buildExpressionParser table (try application <|> term) <?> "expression"

table :: OperatorTable String () (State SourcePos) Expr
table = [
    [
        Infix (InfixExpr Mul <$ operator "*") AssocLeft,
        Infix (InfixExpr Div <$ operator "/") AssocLeft
    ],
    [
        Infix (InfixExpr Add <$ operator "+") AssocLeft,
        Infix (InfixExpr Sub <$ operator "-") AssocLeft
    ],
    [
        Infix (InfixExpr Lte <$ operator "<=") AssocLeft,
        Infix (InfixExpr Lt <$ operator "<") AssocLeft,
        Infix (InfixExpr Gte <$ operator ">=") AssocLeft,
        Infix (InfixExpr Gt <$ operator ">") AssocLeft
    ],
    [
        Infix (InfixExpr Eq <$ operator "==") AssocLeft,
        Infix (InfixExpr Neq <$ operator "!=") AssocLeft
    ]
    ]

application :: IndentParser Expr
application = ApplicationExpr <$> identifier <*> many (sameOrIndented >> term)

term :: IndentParser Expr
term = choice [
    flip ApplicationExpr [] <$> identifier,
    ListExpr <$> brackets (expr `sepEndBy` comma),
    parens expr,
    ValueExpr <$> value
    ] <?> "term"

value :: IndentParser Value
value = choice [
    BoolValue False <$ reserved "false",
    BoolValue True <$ reserved "true",
    IntValue <$> natural,
    NilValue <$ reserved "nil"
    ] <?> "value"

type_ :: IndentParser Type
type_ = choice [
    BoolType <$ reserved "Bool",
    IntType <$ reserved "Int",
    ListType <$> brackets type_,
    NilType <$ reserved "Nil"
    ] <?> "type"

