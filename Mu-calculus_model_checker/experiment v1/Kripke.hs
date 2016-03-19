module Kripke where

import Types
import ParseLib
import List

first :: (a,b,c) -> a
first (a,b,c) = a
second :: (a,b,c) -> b
second (a,b,c) = b
third :: (a,b,c) -> c
third (a,b,c) = c

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
lbls0 = \n -> []

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



