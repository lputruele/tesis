module LangSyntax where

import Types

data Expr =  Bin Op Expr Expr
           | Not Expr
           | Lit Value
           | Ident Name
           deriving Show

data Op = And | Or | Xor | Impl | Iff 
        deriving (Show, Eq)

data Comm = Skip
          | Assign Name Expr
          | MAssign [Name] [Expr]
          | Seq Comm Comm
          | If [(Expr,Comm)] --if con guardas, entra no deterministicamente por alguna condicion verdadera
          | While Expr Comm --while tradicional
          | New Decl Comm
          | PCall Name [Name]
          deriving Show

data Decl = | DVar Name Expr
            | DProc Name [Name] Comm
            | DSeq Decl Decl
          deriving Show


