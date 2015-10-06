
{-|
Module      : Language.Qux.Annotated.Simplify
Description : Simplify functions that return abstract syntax tree nodes without annotations.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Simplify functions that return abstract syntax tree nodes without annotations.
-}

module Language.Qux.Annotated.Simplify (
    -- * Functions
    sId, sProgram, sDecl, sStmt, sExpr, sType
) where

import qualified    Language.Qux.Annotated.Syntax as Ann
import              Language.Qux.Syntax


-- | Simplifies an identifier.
sId :: Ann.Id a -> String
sId (Ann.Id _ id) = id

-- | Simplifies a program.
sProgram :: Ann.Program a -> Program
sProgram (Ann.Program _ module_ decls) = Program (map sId module_) (map sDecl decls)

-- | Simplifies a declaration.
sDecl :: Ann.Decl a -> Decl
sDecl (Ann.FunctionDecl _ name parameters stmts) = FunctionDecl (sId name) (map (tmap sType sId) parameters) (map sStmt stmts)

-- | Simplifies a statement.
sStmt :: Ann.Stmt a -> Stmt
sStmt (Ann.IfStmt _ condition trueStmts falseStmts) = IfStmt (sExpr condition) (map sStmt trueStmts) (map sStmt falseStmts)
sStmt (Ann.ReturnStmt _ expr)                       = ReturnStmt (sExpr expr)
sStmt (Ann.WhileStmt _ condition stmts)             = WhileStmt (sExpr condition) (map sStmt stmts)

-- | Simplifies an expression.
sExpr :: Ann.Expr a -> Expr
sExpr (Ann.ApplicationExpr _ id arguments)  = ApplicationExpr (sId id) (map sExpr arguments)
sExpr (Ann.BinaryExpr _ op lhs rhs)         = BinaryExpr op (sExpr lhs) (sExpr rhs)
sExpr (Ann.ListExpr _ elements)             = ListExpr (map sExpr elements)
sExpr (Ann.UnaryExpr _ op expr)             = UnaryExpr op (sExpr expr)
sExpr (Ann.ValueExpr _ value)               = ValueExpr value

-- | Simplifies a type.
sType :: Ann.Type a -> Type
sType (Ann.BoolType _)          = BoolType
sType (Ann.IntType _)           = IntType
sType (Ann.ListType _ inner)    = ListType $ sType inner
sType (Ann.NilType _)           = NilType


tmap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
tmap f g (a, c) = (f a, g c)

