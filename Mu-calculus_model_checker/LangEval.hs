module LangEval where

import Types
import LangSyntax
import OBDD
import Data.List

{-Expression eval
eeval :: Expr -> OBDD AP
eeval (T n) = OBDD.unit n True
eeval (F n) = OBDD.unit n False
eeval (Lit v) = OBDD.constant v

--Variation for primed variables, also remembers variables of the expression
eeval2 :: Expr -> [AP] -> (OBDD AP, [AP])
eeval2 (And e0 e1) visited = let tmp = eeval2 e0 visited in (OBDD.and[fst tmp, fst tmp2], snd tmp2)
															where tmp2 = eeval2 e1 (snd tmp)
eeval2 (Not e) visited = let tmp = eeval2 e visited in (OBDD.not [fst tmp],snd tmp) 
eeval2 (Lit v) visited = (OBDD.constant v, visited) 
eeval2 (Ident n) visited = (OBDD.unit (n++"'") True,[n] `union` visited)
-}


--Generates OBDD from a model of a program
ceval :: Comm -> Env -> OBDD AP
ceval (Seq c0 c1) env = let (x,y) = (ceval c0 env,ceval c1 env) in OBDD.or[x,y] 
ceval (Rule (e0,e1)) env = let xs = gentrans (filt env e0) [] in 
							  if xs /= [] then 
							    OBDD.or[OBDD.and[mkAnd x, mkAnd2 (fill x e1)]| x <- xs]
							  else 
                                OBDD.and[mkAnd e0, mkAnd2 (fill e0 e1)]

mkAnd :: Env -> OBDD AP
mkAnd [] = OBDD.constant True
mkAnd (x:xs) = if snd x then OBDD.and[OBDD.unit (fst x) True, mkAnd xs]
					  else OBDD.and[OBDD.unit (fst x) False, mkAnd xs]

mkAnd2 :: Env -> OBDD AP
mkAnd2 [] = OBDD.constant True
mkAnd2 (x:xs) = if snd x then OBDD.and[OBDD.unit (fst x++"'") True, mkAnd2 xs]
					   else OBDD.and[OBDD.unit (fst x++"'") False, mkAnd2 xs]

fill :: Env -> Env -> Env
fill [] ys = ys
fill (x:xs) ys = if fst x `notElem` map fst ys then fill xs ([x] `union` ys)
											 else fill xs ys

filt :: Env -> Env -> Env
filt [] ys = []
filt (x:xs) ys = if fst x `notElem` map fst ys then [x] `union` filt xs ys
											 else filt xs ys

gentrans :: Env -> Env -> [Env]
gentrans [] ys = [ys]
gentrans (x:xs) ys = gentrans xs ((fst x,False):ys) ++ gentrans xs ((fst x,True):ys)

--Declaration eval
deval :: Decl -> Env -> Env
deval (DVar n) env = if (n,True) `elem` env then [(n,True)] else [(n,False)]
deval (DSeq d0 d1) env = deval d0 env ++ deval d1 env




