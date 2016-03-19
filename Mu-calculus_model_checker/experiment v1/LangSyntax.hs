module LangSyntax where

import Types

data Expr =  Bin Op Expr Expr
           | Not Expr
           | Lit Bool
           | Ident Name
           deriving Show

data Op = And | Or | Xor | Impl | Iff 
        deriving (Show, Eq)

data Comm = Skip
          | Assign Name Expr
          | Seq Comm Comm
          | If [(Expr,Comm)] --if con guardas, entra no deterministicamente por alguna condicion verdadera
          | While Expr Comm --while tradicional
          | New Decl Comm
          | PCall Name [Name]
          | Dup Comm Comm
          deriving Show

data Decl = DVar Name Expr
            | DProc Name [Name] Comm
            | DSeq Decl Decl
          deriving Show


