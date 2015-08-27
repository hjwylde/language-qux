
{-|
Module      : Language.Qux.Annotated.PrettyPrinter
Description : Pretty instances and rendering functions for Qux language elements.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

"Text.PrettyPrint" instances and rendering functions for Qux language elements.

To render a program, call: @render $ pPrint program@
-}

module Language.Qux.Annotated.PrettyPrinter (
    -- Types
    Pretty(..), Style(..), Mode(..),

    -- * Rendering
    render, renderStyle, renderOneLine
) where

import Language.Qux.Annotated.Simplify
import Language.Qux.Annotated.Syntax
import Language.Qux.PrettyPrinter

import Text.PrettyPrint


instance Pretty (Id a) where
    pPrint = text . sId

instance Pretty (Program a) where
    pPrint = pPrint . sProgram

instance Pretty (Decl a) where
    pPrint = pPrint . sDecl

instance Pretty (Stmt a) where
    pPrint = pPrint . sStmt

instance Pretty (Expr a) where
    pPrint = pPrint . sExpr

instance Pretty (Type a) where
    pPrint = pPrint . sType

