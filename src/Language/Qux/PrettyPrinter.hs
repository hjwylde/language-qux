
{-|
Module      : Language.Qux.PrettyPrinter
Description : Document functions for Qux language elements.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

"Text.PrettyPrint" document functions for Qux language elements.

To render a program, call: @render $ programDoc program@
-}

module Language.Qux.PrettyPrinter (
    -- * Rendering
    render,

    -- * Document functions
    programDoc, declDoc, stmtDoc, exprDoc, infixOpDoc, valueDoc, typeDoc
) where

import Data.Char
import Data.List

import Language.Qux.Ast

import Text.PrettyPrint


-- | 'Program' document
programDoc :: Program -> Doc
programDoc (Program decls) = vcat $ map (($+$ emptyLine) . declDoc) decls

-- | 'Decl' document
declDoc :: Decl -> Doc
declDoc (FunctionDecl name parameters stmts) = vcat [
    text name <+> text "::" <+> parametersDoc <> colon,
    nest 4 (block stmts)
    ]
    where
        parametersDoc = fsep $ punctuate
            (space <> text "->")
            (map (\(t, p) -> typeDoc t <+> (if p == "@" then empty else text p)) parameters)

-- | 'Stmt' document
stmtDoc :: Stmt -> Doc
stmtDoc (IfStmt condition trueStmts falseStmts) = vcat [
    text "if" <+> exprDoc condition <> colon,
    nest 4 (block trueStmts),
    if null falseStmts then empty else text "else:",
    nest 4 (block falseStmts)
    ]
stmtDoc (ReturnStmt expr)                       = text "return" <+> exprDoc expr
stmtDoc (WhileStmt condition stmts)             = vcat [
    text "while" <+> exprDoc condition <> colon,
    nest 4 (block stmts)
    ]

-- | 'Expr' document
exprDoc :: Expr -> Doc
exprDoc (ApplicationExpr name arguments)    = text name <+> fsep (map exprDoc arguments)
-- TODO (hjw): don't use so many parenthesis
exprDoc (InfixExpr op lhs rhs)              = parens $ fsep [exprDoc lhs, infixOpDoc op, exprDoc rhs]
exprDoc (ListExpr elements)                 = brackets $ fsep (punctuate comma (map exprDoc elements))
exprDoc (ValueExpr value)                   = valueDoc value

-- | 'InfixOp' document
infixOpDoc :: InfixOp -> Doc
infixOpDoc Acc  = text "!!"
infixOpDoc Mul  = text "*"
infixOpDoc Div  = text "/"
infixOpDoc Mod  = text "%"
infixOpDoc Add  = text "+"
infixOpDoc Sub  = text "-"
infixOpDoc Lt   = text "<"
infixOpDoc Lte  = text "<="
infixOpDoc Gt   = text ">"
infixOpDoc Gte  = text ">="
infixOpDoc Eq   = text "=="
infixOpDoc Neq  = text "!="

-- | 'Value' document
valueDoc :: Value -> Doc
valueDoc (BoolValue bool)       = text $ map toLower (show bool)
valueDoc (IntValue int)         = text $ show int
valueDoc (ListValue elements)   = brackets $ fsep (punctuate comma (map valueDoc elements))
valueDoc NilValue               = text "nil"

-- | 'Type' document
typeDoc :: Type -> Doc
typeDoc BoolType            = text "Bool"
typeDoc IntType             = text "Int"
typeDoc (ListType type_)    = brackets $ typeDoc type_
typeDoc NilType             = text "Nil"


block :: [Stmt] -> Doc
block = vcat . (map stmtDoc)

emptyLine = text ""

