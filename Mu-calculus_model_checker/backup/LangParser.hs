module LangParser where

import ParseLib
import LangSyntax

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
pAssign = do {n <- myident; symbol ":="; e <- pExpr; return (Assign n e)}

pExprs :: Parser [Expr]
pExprs = pExpr `sepby1` symbol ","

pIf :: Parser Comm
pIf = do {symbol "if"; guards <- [pGuard]; symbol "fi"; return (If guards)}

pWhile :: Parser Comm
pDo = do {symbol "while"; e <- pBExpr; symbol "do"; c <- PComm; symbol "od"; return (While e c)}

pGuard :: Parser (Expr,Comm)
pGuard = do {symbol "::"; e <- pBExpr; symbol "->"; c <- pComm; return (e,c)}

pSemi = symbol ";" >> return Seq
pPipe = symbol "|" >> return Dup

-- Declaration parsers

pDSemi = symbol ";" >> return DSeq

pDecl :: Parser Decl
pDecl = pAtomicDecl `chainl1` pDSemi

pAtomicDecl :: Parser Decl
pAtomicDecl = pVarDecl +++ pConDecl +++ pProcDecl

pVarDecl :: Parser Decl
pVarDecl = do {symbol "var"; nes <- pVarsDecls; return (newvars nes)}

pVarsDecls :: Parser [(Name,Expr)]
pVarsDecls = do {n <- myident; e <- pVarInit; return (n,e)} `sepby1` pDSemi

pVarInit :: Parser Expr
pVarInit = do {symbol ":="; pExpr} +++ return (Lit (IV 0)) -- hmm, should do something else if it is not int

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
pConjuncts = pRel `chainl1` pAndOperator

pLogicTopOperator = (symbol "<=>" >> return (Bin Iff)) +++
                    (symbol "=>" >> return (Bin Impl)) +++
                    (symbol "<!>" >> return (Bin Xor))
pOrOperator = symbol "|" >> return (Bin Or)
pAndOperator = symbol "&" >> return (Bin And)

pRel :: Parser Expr 
pRel = pIExpr `chainl1` pRelOperator

pRelOperator = (symbol "==" >> return (Bin Equal)) +++
               (symbol "!=" >> return (Bin Nequal)) +++
               (symbol "<" >> return (Bin Less)) +++
               (symbol ">" >> return (Bin Greater)) +++
               (symbol "<=" >> return (Bin LessEq)) +++
               (symbol ">=" >> return (Bin GreaterEq))

-- Integer Expression parsers

pIExpr :: Parser Expr
pIExpr = pTerm `chainl1` pTermOperator

pTerm :: Parser Expr
pTerm = pFactor `chainl1` pFactorOperator

pFactor :: Parser Expr
pFactor = do {v <- pLiteral; return (Lit v)} +++
          pVarOrMore +++
          do {symbol "!"; b <- pRel; return (Not b)} +++
          do {symbol "-"; e <- pTerm; return (Opp e)} +++
          bracket (symbol "(") pExpr (symbol ")")

pTermOperator = (symbol "+" >> return (Bin Plus)) +++
                (symbol "-" >> return (Bin Minus))
pFactorOperator = (symbol "*" >> return (Bin Times)) +++
                  (symbol "/" >> return (Bin Div)) +++
                  (symbol "%" >> return (Bin Mod))

pLiteral :: Parser Value
pLiteral = do {i <- natural; return (IV i)} +++
           (symbol "true" >> return (BV True)) +++
           (symbol "false" >> return (BV False))

pVarOrMore :: Parser Expr
pVarOrMore = do {n <- myident; f <- pIsThereMore; return (f n)}

pIsThereMore :: Parser (Name -> Expr)
pIsThereMore = do {symbol "("; es <- pExprs; symbol ")"; return (\n -> FCall n es)} +++
               return Ident

reserved_words :: [Name]
reserved_words = ["do", "od", "if", "fi", "skip", "true", "false", "end", "new", "var", "proc", "con", "in", "break", "continue"]

myident :: Parser Name
myident = identifier reserved_words



