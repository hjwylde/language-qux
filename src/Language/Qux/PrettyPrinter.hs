
{-|
Module      : Language.Qux.PrettyPrinter
Description : Pretty instances and rendering functions for Qux language elements.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

"Text.PrettyPrint" instances and rendering functions for Qux language elements.

To render a program, call: @render $ pPrint program@
-}

module Language.Qux.PrettyPrinter (
    -- * Types
    Pretty(..), Style(..), Mode(..),

    -- * Rendering
    render, renderStyle, renderOneLine
) where

import Data.Char (toLower)

import Language.Qux.Syntax

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass


-- TODO (hjw): use maybeParens to avoid using so many parenthesis

-- | Like 'render', but renders the doc on one line.
renderOneLine :: Doc -> String
renderOneLine = renderStyle (style { mode = OneLineMode })


instance Pretty Doc where
    pPrint = id

instance Pretty Program where
    pPrint (Program decls) = vcat $ map (($+$ emptyLine) . pPrint) decls

instance Pretty Decl where
    pPrint (FunctionDecl name parameters stmts) = vcat [
        text name <+> text "::" <+> parametersDoc <> colon,
        nest 4 (block stmts)
        ]
        where
            parametersDoc = fsep $ punctuate
                (space <> text "->")
                (map (\(t, p) -> pPrint t <+> (if p == "@" then empty else text p)) parameters)

instance Pretty Stmt where
    pPrint (IfStmt condition trueStmts falseStmts)  = vcat [
        text "if" <+> pPrint condition <> colon,
        nest 4 (block trueStmts),
        if null falseStmts then empty else text "else:",
        nest 4 (block falseStmts)
        ]
    pPrint (ReturnStmt expr)                        = text "return" <+> pPrint expr
    pPrint (WhileStmt condition stmts)              = vcat [
        text "while" <+> pPrint condition <> colon,
        nest 4 (block stmts)
        ]

instance Pretty Expr where
    pPrint (ApplicationExpr name arguments) = text name <+> fsep (map pPrint arguments)
    pPrint (BinaryExpr op lhs rhs)          = parens $ fsep [pPrint lhs, pPrint op, pPrint rhs]
    pPrint (ListExpr elements)              = brackets $ fsep (punctuate comma (map pPrint elements))
    pPrint (UnaryExpr Len expr)             = pipes $ pPrint expr
    pPrint (UnaryExpr op expr)              = pPrint op <> pPrint expr
    pPrint (ValueExpr value)                = pPrint value

instance Pretty BinaryOp where
    pPrint Acc = text "!!"
    pPrint Mul = text "*"
    pPrint Div = text "/"
    pPrint Mod = text "%"
    pPrint Add = text "+"
    pPrint Sub = text "-"
    pPrint Lt  = text "<"
    pPrint Lte = text "<="
    pPrint Gt  = text ">"
    pPrint Gte = text ">="
    pPrint Eq  = text "=="
    pPrint Neq = text "!="

instance Pretty UnaryOp where
    pPrint Len = text "length"
    pPrint Neg = text "-"

instance Pretty Value where
    pPrint (BoolValue bool)     = text $ map toLower (show bool)
    pPrint (IntValue int)       = text $ show int
    pPrint (ListValue elements) = brackets $ fsep (punctuate comma (map pPrint elements))
    pPrint NilValue             = text "nil"

instance Pretty Type where
    pPrint BoolType         = text "Bool"
    pPrint IntType          = text "Int"
    pPrint (ListType inner) = brackets $ pPrint inner
    pPrint NilType          = text "Nil"


block :: [Stmt] -> Doc
block = vcat . (map pPrint)

emptyLine :: Doc
emptyLine = text ""

pipes :: Doc -> Doc
pipes doc = char '|' <> doc <> char '|'

