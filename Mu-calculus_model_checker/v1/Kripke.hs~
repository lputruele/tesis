module Kripke where

import Types
import ParseLib
import List


--Environment
env0 :: Env
env0 = \n -> []

setE :: (Name,[State]) -> Env -> Env
setE (n,v) env = \m -> if m == n then v else getE m env 

getE :: Name -> Env -> [State]
getE n env = env n

msetE :: [(Name,[State])] -> Env -> Env
msetE nds env = foldl (flip setE) env nds

mgetE :: [Name] -> Env -> [(Name,[State])]
mgetE ns env = map (\n -> (n,getE n env)) ns

iniE :: [(Name,[State])] -> Env
iniE nds = msetE nds env0

--Labels
lbls0 :: Labels
lbls0 = \n -> return []

setL :: (State,[AP]) -> Labels -> Labels
setL (n,v) lbls = \m -> if m == n then v else getL m lbls 

getL :: State -> Labels -> [AP]
getL n lbls = lbls n

msetL :: [(State,[AP])] -> Labels -> Labels
msetL nds lbls = foldl (flip setL) lbls nds

mgetL :: [State] -> Labels -> [(State,[AP])]
mgetL ns lbls = map (\n -> (n,getL n lbls)) ns

iniL :: [(State,[AP])] -> Labels
iniL nds = msetL nds lbls0


--parser kripke
pKripke :: Parser Kripke
pKripke = do {symbol "S"; symbol "="; symbol "{"; ss <- pNaturals; symbol "}"; symbol "T"; symbol "="; symbol "{"; ts <- pTrans; symbol "}"; symbol "L"; symbol ":"; ls <- pLabels; return  (ss, ts, (iniL ls))}

pIdentifiers :: Parser [String]
pIdentifiers = identifier [] `sepby1` symbol ","

pNaturals :: Parser [State]
pNaturals = natural `sepby1` symbol ","

pTran :: Parser Transition
pTran = do {symbol "{"; n0 <- natural; symbol ","; n1 <- natural; symbol "}";  return (n0,n1)}

pTrans :: Parser [Transition]
pTrans = pTran `sepby1` symbol ","

pLabel :: Parser (State,[AP])
pLabel = do {symbol "L"; symbol "("; n <- natural; symbol ")"; symbol "="; symbol "{"; ps <- pIdentifiers; symbol "}"; return (n,ps)}

pLabels :: Parser [(State,[AP])]
pLabels = pLabel `sepby1` symbol ","


