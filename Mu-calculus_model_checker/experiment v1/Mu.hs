module Mu where

import List
import ParseLib
import Types
import Kripke

--syntax
data Form = Var Name
         | Prop AP
         | And Form Form
         | Or Form Form
         | Not Form
         | Box Form 
         | Diamond Form 
         | Nu Name Form --Greater fix-point operator
         | Mu Name Form --Lower fix-point operator
         deriving Eq

--printer
instance Show Form where
  show (Var n) = n
  show (Prop p) = fst p
  show (And f0 f1) = "(" ++ show f0 ++ " ^ " ++ show f1 ++ ")"
  show (Or f0 f1) = "(" ++ show f0 ++ " v " ++ show f1 ++ ")"
  show (Not f) = "¬" ++ show f
  show (Box f) = "[" ++ "]" ++ show f
  show (Diamond f) = "<" ++ ">" ++ show f
  show (Nu n f) = "V" ++ n ++ "." ++ show f
  show (Mu n f) = "U" ++ n ++ "." ++ show f

sshowList :: [Form] -> String
sshowList [] = ""
sshowList [x] = show x
sshowList (x:xs) = show x ++ "," ++ sshowList xs

--parser

pForm :: Parser Form
pForm = pVar +++ pProp +++ pNot +++ pBox +++ pDiamond +++ pNu +++ pMu +++ bracket (symbol "(") (pAnd +++ pOr) (symbol ")")

pVar :: Parser Form
pVar = do {symbol ":"; n <- identifier []; return (Var n)}

pProp :: Parser Form
pProp = do {p <- identifier []; b <- pLiteral; return (Prop (p,b))}

pNot :: Parser Form
pNot = do {symbol "¬"; f <- pForm; return (Not f)}

pAnd :: Parser Form
pAnd = do {f0 <- pForm; symbol "^"; f1 <- pForm; return (And f0 f1)}

pOr :: Parser Form
pOr = do {f0 <- pForm; symbol "v"; f1 <- pForm; return (Or f0 f1)}

pBox :: Parser Form
pBox = do {symbol "["; symbol "]"; f <- pForm; return (Box f)}

pDiamond :: Parser Form
pDiamond = do {symbol "<"; symbol ">"; f <- pForm; return (Diamond f)}

pNu :: Parser Form
pNu = do {symbol "V"; n <- identifier[]; symbol "."; f <- pForm; return (Nu n f)}

pMu :: Parser Form
pMu = do {symbol "U"; n <- identifier[]; symbol "."; f <- pForm; return (Mu n f)}

pLiteral :: Parser Bool
pLiteral = (symbol "true" >> return (True)) +++
           (symbol "false" >> return (False))

pForms :: Parser [Form]
pForms = pForm `sepby1` symbol ","
