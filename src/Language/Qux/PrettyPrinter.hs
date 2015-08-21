
module Language.Qux.PrettyPrinter (
    programDoc, declDoc, stmtDoc, exprDoc, valueDoc, typeDoc, infixOpDoc
) where

import Data.Char
import Data.List

import Language.Qux.Ast

import Text.PrettyPrint


programDoc :: Program -> Doc
programDoc (Program decls) = vcat $ map (($+$ emptyLine) . declDoc) decls

declDoc :: Decl -> Doc
declDoc (FunctionDecl name parameters stmts) = vcat [
    text name <+> text "::" <+> parametersDoc <> colon,
    nest 4 (block stmts)
    ]
    where
        parametersDoc = fsep $ punctuate
            (space <> rightArrow)
            (map (\(t, p) -> typeDoc t <+> (if p == "@" then empty else text p)) parameters)

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

exprDoc :: Expr -> Doc
exprDoc (ApplicationExpr name arguments)    = text name <+> fsep (map exprDoc arguments)
exprDoc (InfixExpr op lhs rhs)              = parens $ fsep [exprDoc lhs, infixOpDoc op, exprDoc rhs]
exprDoc (ListExpr elements)                 = brackets $ fsep (punctuate comma (map exprDoc elements))
exprDoc (ValueExpr value)                   = valueDoc value

valueDoc :: Value -> Doc
valueDoc (BoolValue bool)       = text $ map toLower (show bool)
valueDoc (IntValue int)         = text $ show int
valueDoc (ListValue elements)   = brackets $ fsep (punctuate comma (map valueDoc elements))
valueDoc NilValue               = text "nil"

typeDoc :: Type -> Doc
typeDoc BoolType            = text "Bool"
typeDoc IntType             = text "Int"
typeDoc (ListType type_)    = brackets $ typeDoc type_
typeDoc NilType             = text "Nil"

infixOpDoc :: InfixOp -> Doc
infixOpDoc Add  = text "+"
infixOpDoc Sub  = text "-"
infixOpDoc Mul  = text "*"
infixOpDoc Div  = text "/"
infixOpDoc Eq   = text "=="
infixOpDoc Neq  = text "!="
infixOpDoc Lt   = text "<"
infixOpDoc Lte  = text "<="
infixOpDoc Gt   = text ">"
infixOpDoc Gte  = text ">="


block :: [Stmt] -> Doc
block = vcat . (map stmtDoc)

emptyLine = text ""

rightArrow = text "->"

