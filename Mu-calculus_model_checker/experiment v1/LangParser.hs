module LangParser where

import ParseLib
import LangSyntax
import Types

-- Command parsers

pComm :: Parser Comm
pComm = pCommNoSeq `chainl1` pSemi

pCommNoSeq :: Parser Comm
pCommNoSeq = pSkip +++ pWhile +++ pIf +++ pAssign +++ pNew  +++ pPCall +++ pDup

pDup :: Parser Comm
pDup = pPCall `chainl1` pPipe

pPCall :: Parser Comm
pPCall = do {pn <- myident; symbol "("; as <- myident `sepby` symbol ","; symbol ")"; return (PCall pn as)}

pNew :: Parser Comm
pNew = do {symbol "new"; d <- pDecl; symbol "in"; symbol "{"; c <- pComm; symbol "}"; return (New d c)}

pSkip :: Parser Comm
pSkip = symbol "skip" >> return Skip

pAssign :: Parser Comm
pAssign = do {n <- myident; symbol "="; e <- pBExpr; return (Assign n e)}

pExprs :: Parser [Expr]
pExprs = pBExpr `sepby1` symbol ","

pIf :: Parser Comm
pIf = do {symbol "if"; guards <- [pGuard]; symbol "fi"; return (If guards)}

pWhile :: Parser Comm
pWhile = do {symbol "while"; e <- pBExpr; symbol "do"; c <- pComm; symbol "od"; return (While e c)}

pGuard :: Parser (Expr,Comm)
pGuard = do {symbol "::"; e <- pBExpr; symbol "->"; c <- pComm; return (e,c)}

pSemi = symbol ";" >> return Seq
pPipe = symbol "||" >> return Dup

-- Declaration parsers

pDSemi = symbol ";" >> return DSeq

pDecl :: Parser Decl
pDecl = pAtomicDecl `chainl1` pDSemi

pAtomicDecl :: Parser Decl
pAtomicDecl = pVarDecl +++ pProcDecl

pVarDecl :: Parser Decl
pVarDecl = do {symbol "var"; nes <- pVarsDecls; return (newvars nes)}

pVarsDecls :: Parser [(Name,Expr)]
pVarsDecls = do {n <- myident; e <- pVarInit; return (n,e)} `sepby1` pDSemi

pVarInit :: Parser Expr
pVarInit = do {symbol ":="; pBExpr} +++ return (Lit False) 

newvars :: [(Name,Expr)] -> Decl
newvars nes = foldr1 DSeq (map (\(n,e) -> DVar n e) nes)

pProcDecl :: Parser Decl
pProcDecl = do {symbol "proc"; n <- myident; symbol "("; params <- pParams; symbol ")"; symbol "{"; c <- pComm; symbol "}"; return (DProc n params c)}

pParams :: Parser [Name]
pParams = myident `sepby1` symbol ","


-- Boolean Expression parsers

pBExpr :: Parser Expr
pBExpr = pDisjuncts `chainl1` pLogicTopOperator

pDisjuncts :: Parser Expr
pDisjuncts = pConjuncts `chainl1` pOrOperator

pConjuncts :: Parser Expr
pConjuncts = pFactor `chainl1` pAndOperator

pFactor :: Parser Expr
pFactor = pLiteral +++
          pVar +++
          do {symbol "!"; b <- pFactor; return (Not b)} +++
          bracket (symbol "(") pBExpr (symbol ")")

pLogicTopOperator = (symbol "==" >> return (Bin Iff)) +++
                    (symbol "=>" >> return (Bin Impl)) +++
                    (symbol "/" >> return (Bin Xor))
pOrOperator = symbol "|" >> return (Bin Or)
pAndOperator = symbol "&" >> return (Bin And)

pLiteral :: Parser Expr
pLiteral = (symbol "true" >> return (Lit True)) +++
           (symbol "false" >> return (Lit False))

pVar :: Parser Expr
pVar = do {n <- myident; return (Ident n)}


reserved_words :: [Name]
reserved_words = ["do", "od", "if", "fi", "skip", "true", "false", "end", "new", "var", "proc", "con", "in", "break", "continue"]

myident :: Parser Name
myident = identifier reserved_words



