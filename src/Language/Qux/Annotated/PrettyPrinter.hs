
{-|
Module      : Language.Qux.Annotated.PrettyPrinter
Description : Pretty instances and rendering functions for Qux language elements.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

"Text.PrettyPrint" instances and rendering functions for Qux language elements.

To render a program, call: @render $ pPrint program@
-}

module Language.Qux.Annotated.PrettyPrinter where

import Language.Qux.Annotated.Syntax

import Text.PrettyPrint.HughesPJClass


instance Pretty (Id a) where
    pPrint = text . simp

instance Pretty (Program a) where
    pPrint = pPrint . simp

instance Pretty (Decl a) where
    pPrint = pPrint . simp

instance Pretty (Stmt a) where
    pPrint = pPrint . simp

instance Pretty (Expr a) where
    pPrint = pPrint . simp

instance Pretty (Type a) where
    pPrint = pPrint . simp

