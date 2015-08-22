
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
    render, renderOneLine,

    -- * Document functions
    programDoc, declDoc, stmtDoc, exprDoc, binaryOpDoc, valueDoc, typeDoc
) where

import Data.Char
import Data.List

import Language.Qux.Ast

import Text.PrettyPrint


-- | Renders the 'Doc' to a 'String' on one line.
renderOneLine :: Doc -> String
renderOneLine = renderStyle (style { mode = OneLineMode })


-- | 'Program' document.
programDoc :: Program -> Doc
programDoc (Program decls) = vcat $ map (($+$ emptyLine) . declDoc) decls

-- | 'Decl' document.
declDoc :: Decl -> Doc
declDoc (FunctionDecl name parameters stmts) = vcat [
    text name <+> text "::" <+> parametersDoc <> colon,
    nest 4 (block stmts)
    ]
    where
        parametersDoc = fsep $ punctuate
            (space <> text "->")
            (map (\(t, p) -> typeDoc t <+> (if p == "@" then empty else text p)) parameters)

-- | 'Stmt' document.
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

-- | 'Expr' document.
exprDoc :: Expr -> Doc
exprDoc (ApplicationExpr name arguments)    = text name <+> fsep (map exprDoc arguments)
-- TODO (hjw): don't use so many parenthesis
exprDoc (BinaryExpr op lhs rhs)             = parens $ fsep [exprDoc lhs, binaryOpDoc op, exprDoc rhs]
exprDoc (ListExpr elements)                 = brackets $ fsep (punctuate comma (map exprDoc elements))
exprDoc (UnaryExpr Len expr)                = pipes $ exprDoc expr
exprDoc (UnaryExpr op expr)                 = unaryOpDoc op <> exprDoc expr
exprDoc (ValueExpr value)                   = valueDoc value

-- | 'BinaryOp' document.
binaryOpDoc :: BinaryOp -> Doc
binaryOpDoc Acc = text "!!"
binaryOpDoc Mul = text "*"
binaryOpDoc Div = text "/"
binaryOpDoc Mod = text "%"
binaryOpDoc Add = text "+"
binaryOpDoc Sub = text "-"
binaryOpDoc Lt  = text "<"
binaryOpDoc Lte = text "<="
binaryOpDoc Gt  = text ">"
binaryOpDoc Gte = text ">="
binaryOpDoc Eq  = text "=="
binaryOpDoc Neq = text "!="

-- | 'UnaryOp' document.
unaryOpDoc :: UnaryOp -> Doc
unaryOpDoc Neg = text "-"

-- | 'Value' document.
valueDoc :: Value -> Doc
valueDoc (BoolValue bool)       = text $ map toLower (show bool)
valueDoc (IntValue int)         = text $ show int
valueDoc (ListValue elements)   = brackets $ fsep (punctuate comma (map valueDoc elements))
valueDoc NilValue               = text "nil"

-- | 'Type' document.
typeDoc :: Type -> Doc
typeDoc BoolType            = text "Bool"
typeDoc IntType             = text "Int"
typeDoc (ListType type_)    = brackets $ typeDoc type_
typeDoc NilType             = text "Nil"


block :: [Stmt] -> Doc
block = vcat . (map stmtDoc)

emptyLine = text ""

pipes doc = char '|' <> doc <> char '|'

