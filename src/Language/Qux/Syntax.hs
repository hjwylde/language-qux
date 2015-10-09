
{-|
Module      : Language.Qux.Syntax
Description : Abstract syntax tree nodes.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Abstract syntax tree nodes.
Instances of 'Pretty' are provided for pretty printing.
-}

module Language.Qux.Syntax (
    -- * Nodes
    Id, Program(..), Decl(..), Stmt(..), Expr(..), BinaryOp(..), UnaryOp(..), Value(..), Type(..)
) where

import Data.Char (toLower)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass


-- | An identifier. Identifiers should match '[a-z_][a-zA-Z0-9_']*'.
type Id = String


-- | A program is a module identifier (list of 'Id''s) and a list of declarations.
data Program = Program [Id] [Decl]
    deriving (Eq, Show)

instance Pretty Program where
    pPrint (Program module_ decls) = vcat $ map ($+$ text "") ([
        text "module" <+> hcat (punctuate (char '.') (map text module_))
        ] ++ map pPrint decls)


-- | A declaration.
data Decl = FunctionDecl Id [(Type, Id)] [Stmt] -- ^ A name, list of ('Type', 'Id') parameters and statements.
                                                --   The return type is treated as a parameter with id '@'.
    deriving (Eq, Show)

instance Pretty Decl where
    pPrint (FunctionDecl name parameters stmts) = vcat [
        text name <+> text "::" <+> parametersDoc <> colon,
        nest 4 (block stmts)
        ]
        where
            parametersDoc = fsep $ punctuate
                (space <> text "->")
                (map (\(t, p) -> pPrint t <+> (if p == "@" then empty else text p)) parameters)


-- | A statement.
data Stmt   = IfStmt Expr [Stmt] [Stmt] -- ^ A condition, true block and false block of statements.
            | ReturnStmt Expr           -- ^ An expression.
            | WhileStmt Expr [Stmt]     -- ^ A condition and block of statements.
    deriving (Eq, Show)

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


-- | A complex expression.
data Expr   = ApplicationExpr Id [Expr]     -- ^ A function name to call and the arguments to pass as parameters.
            | BinaryExpr BinaryOp Expr Expr -- ^ A binary operation.
            | ListExpr [Expr]               -- ^ A list expression.
            | UnaryExpr UnaryOp Expr        -- ^ A unary operation.
            | ValueExpr Value               -- ^ A raw value.
    deriving (Eq, Show)

instance Pretty Expr where
    pPrint (ApplicationExpr name arguments) = text name <+> fsep (map pPrint arguments)
    pPrint (BinaryExpr op lhs rhs)          = parens $ fsep [pPrint lhs, pPrint op, pPrint rhs]
    pPrint (ListExpr elements)              = brackets $ fsep (punctuate comma (map pPrint elements))
    pPrint (UnaryExpr Len expr)             = char '|' <> pPrint expr <> char '|'
    pPrint (UnaryExpr op expr)              = pPrint op <> pPrint expr
    pPrint (ValueExpr value)                = pPrint value


-- | A binary operator.
data BinaryOp   = Acc -- ^ List access.
                | Mul -- ^ Multiplicaiton.
                | Div -- ^ Division.
                | Mod -- ^ Modulus.
                | Add -- ^ Addition.
                | Sub -- ^ Subtraction.
                | Lt  -- ^ Less than.
                | Lte -- ^ Less than or equal to.
                | Gt  -- ^ Greater than.
                | Gte -- ^ Greater than or equal to.
                | Eq  -- ^ Equal to.
                | Neq -- ^ Not equal to.
    deriving (Eq, Show)

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


-- | A unary operator.
data UnaryOp    = Len -- ^ List length.
                | Neg -- ^ Negation.
    deriving (Eq, Show)

-- | A value is considered to be in it's normal form.
instance Pretty UnaryOp where
    pPrint Len = text "length"
    pPrint Neg = text "-"


data Value  = BoolValue Bool    -- ^ A boolean.
            | IntValue Integer  -- ^ An unbounded integer.
            | ListValue [Value] -- ^ A normalised list value.
            | NilValue          -- ^ A unit value.
    deriving (Eq, Show)

instance Pretty Value where
    pPrint (BoolValue bool)     = text $ map toLower (show bool)
    pPrint (IntValue int)       = text $ show int
    pPrint (ListValue elements) = brackets $ fsep (punctuate comma (map pPrint elements))
    pPrint NilValue             = text "nil"


-- | A type.
data Type   = BoolType
            | IntType
            | ListType Type -- ^ A list type with an inner type.
            | NilType
    deriving (Eq, Show)

instance Pretty Type where
    pPrint BoolType         = text "Bool"
    pPrint IntType          = text "Int"
    pPrint (ListType inner) = brackets $ pPrint inner
    pPrint NilType          = text "Nil"


-- Helper methods

block :: [Stmt] -> Doc
block = vcat . (map pPrint)

