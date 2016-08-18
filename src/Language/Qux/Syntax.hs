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
    Id, Program(..), Decl(..), Attribute(..), Stmt(..), Expr(..), BinaryOp(..),
    UnaryOp(..), Value(..), Type(..),

    -- * Extra methods

    -- ** Utility
    qualify, mangle,

    -- ** Rendering
    pShow,
) where

import Data.List.Extra

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

-- | An identifier. Identifiers should match '[a-z_][a-zA-Z0-9_']*'.
type Id = String

-- | A program is a module identifier (list of 'Id''s) and a list of declarations.
data Program = Program [Id] [Decl]
    deriving (Eq, Show)

instance Pretty Program where
    pPrint (Program module_ decls) = vcat . map ($+$ text "") $
        (text "module" <+> hcat (punctuate (char '.') (map text module_))) : map pPrint decls

-- | A declaration.
data Decl   = FunctionDecl [Attribute] Id [(Type, Id)] [Stmt]   -- ^ A name, list of ('Type', 'Id') parameters and statements.
                                                                --   The return type is treated as a parameter with id '@'.
            | ImportDecl [Id]                                   -- ^ A module identifier to import.
            | TypeDecl [Attribute] Id                           -- ^ A type declaration.
    deriving (Eq, Show)

instance Pretty Decl where
    pPrint (FunctionDecl attrs name type_ [])       = declarationDoc attrs name type_
    pPrint (FunctionDecl attrs name type_ stmts)    = vcat
        [ declarationDoc attrs name type_ <> colon
        , nest 4 $ block stmts
        ]
    pPrint (ImportDecl id)                          = text "import" <+> hcat (punctuate (char '.') (map text id))
    pPrint (TypeDecl attrs name)                    = text "type" <+> hsep (map pPrint attrs) <+> text name

declarationDoc :: [Attribute] -> Id -> [(Type, Id)] -> Doc
declarationDoc attrs name type_ = hsep $ map pPrint attrs ++ [text name, text "::", functionTypeDoc]
    where
        functionTypeDoc = fsep $ punctuate
            (text " ->")
            [pPrint t <+> if p == "@" then empty else text p | (t, p) <- type_]

-- | A declaration attribute.
data Attribute = External
    deriving (Eq, Show)

instance Pretty Attribute where
    pPrint = text . lower . show

-- | A statement.
data Stmt   = IfStmt Expr [Stmt] [Stmt] -- ^ A condition, true block and false block of statements.
            | ReturnStmt Expr           -- ^ An expression.
            | WhileStmt Expr [Stmt]     -- ^ A condition and block of statements.
    deriving (Eq, Show)

instance Pretty Stmt where
    pPrint (IfStmt condition trueStmts falseStmts)  = vcat
        [ text "if" <+> pPrint condition <> colon
        , nest 4 $ block trueStmts
        , if null falseStmts then empty else text "else:"
        , nest 4 $ block falseStmts
        ]
    pPrint (ReturnStmt expr)                        = text "return" <+> pPrint expr
    pPrint (WhileStmt condition stmts)              = vcat
        [ text "while" <+> pPrint condition <> colon
        , nest 4 $ block stmts
        ]

-- | A complex expression.
data Expr   = ApplicationExpr Id [Expr]     -- ^ A function name (unresolved) to call and the
                                            --   arguments to pass as parameters.
            | BinaryExpr BinaryOp Expr Expr -- ^ A binary operation.
            | CallExpr [Id] [Expr]          -- ^ A function identifier (resolved) to call and the
                                            --   arguments to pass as parameters.
            | ListExpr [Expr]               -- ^ A list expression.
            | TypedExpr Type Expr           -- ^ A typed expression.
                                            --   See "Language.Qux.Annotated.TypeResolver".
            | UnaryExpr UnaryOp Expr        -- ^ A unary operation.
            | ValueExpr Value               -- ^ A raw value.
            | VariableExpr Id               -- ^ A local variable access.
    deriving (Eq, Show)

instance Pretty Expr where
    pPrint (ApplicationExpr name arguments) = text name <+> fsep (map pPrint arguments)
    pPrint (BinaryExpr op lhs rhs)          = parens $ fsep [pPrint lhs, pPrint op, pPrint rhs]
    pPrint (CallExpr id arguments)          = text (qualify id) <+> fsep (map pPrint arguments)
    pPrint (ListExpr elements)              = brackets $ fsep (punctuate comma (map pPrint elements))
    pPrint (TypedExpr _ expr)               = pPrint expr
    pPrint (UnaryExpr Len expr)             = char '|' <> pPrint expr <> char '|'
    pPrint (UnaryExpr op expr)              = pPrint op <> pPrint expr
    pPrint (ValueExpr value)                = pPrint value
    pPrint (VariableExpr name)              = text name

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

instance Pretty UnaryOp where
    pPrint Len = text "length"
    pPrint Neg = text "-"

-- | A value is considered to be in it's normal form.
data Value  = BoolValue Bool    -- ^ A boolean.
            | CharValue Char    -- ^ A character.
            | IntValue Integer  -- ^ An unbounded integer.
            | ListValue [Value] -- ^ A normalised list value.
            | NilValue          -- ^ A unit value.
    deriving (Eq, Show)

instance Pretty Value where
    pPrint (BoolValue bool)     = text $ lower (show bool)
    pPrint (CharValue c)        = char c
    pPrint (IntValue int)       = text $ show int
    pPrint (ListValue elements) = brackets $ fsep (punctuate comma (map pPrint elements))
    pPrint NilValue             = text "nil"

-- | A type.
data Type   = BoolType
            | CharType
            | IntType
            | ListType Type -- ^ A list type with an inner type.
            | NilType
    deriving (Eq, Show)

instance Pretty Type where
    pPrint BoolType         = text "Bool"
    pPrint CharType         = text "Char"
    pPrint IntType          = text "Int"
    pPrint (ListType inner) = brackets $ pPrint inner
    pPrint NilType          = text "Nil"

-- | Qualifies the identifier into a single 'Id' joined with periods.
qualify :: [Id] -> Id
qualify = intercalate "."

-- | Mangles the identifier into a single 'Id' joined with underscores.
mangle :: [Id] -> Id
mangle = intercalate "_"

-- | @pShow a@ pretty prints @a@ using a rending mode of 'OneLineMode'.
pShow :: Pretty a => a -> String
pShow = renderStyle (style { mode = OneLineMode }) . pPrint

block :: [Stmt] -> Doc
block = vcat . map pPrint
