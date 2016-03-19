module LangEval where

import Types
import Lang
import Env
import Kripke
import System.Random

--Expression eval
eeval :: Expr -> ProgEnv -> Bool
eeval (Bin op e0 e1) env  = (eeval e0) `operate` (eeval e1)
                          where operate = opBool op
eeval (Not e) env =  not (eeval e)
eeval (Lit v) env = v
eeval (Ident n) env = getE n env

--Expression operators
opBool :: Op -> Bool -> Bool -> Bool
opBool And = (&&)
opBool Or = (||)
opBool Xor = (/=)
opBool Impl = (<=)
opBool Iff = (==)

--Auxiliary functions
evalGuards :: [(Expr,Comm)] -> Kripke -> State -> (Kripke,State)
evalGuards [] m s = (m,s) 
evalGuards (z:zs) m s = let (x,y) = ceval (snd z) (first m ++ [s+1], 
												   second m ++ (s,s+1),
													setL (s+1,getL s (third m))(third m)) s+1 in evalGuards zs m y+1

--Command eval
--takes a Lang program and makes an OBDD out of it
ceval :: Comm -> Kripke -> State{-current state-} -> (Kripke,State) 
ceval Skip m s = (m,getL s (third m)) 
ceval (Assign n e) m s = let x = setE(n,eeval e (getL s (third m))) in  ((first m ++ [s+1],
																  		second m ++ (s,s+1),
																  		setL (s+1,x)(third m)),s+1) 
ceval (Seq c0 c1) m s = let (x,y) = ceval c0 m s in ceval c1 x y
ceval (If xs) m s = let ys = [ x | x <- xs, eeval (fst x) (getL s (third m)) == True] in 
                                evalGuards ys m s                      
                                                                                

ceval (While e c) m s = if eeval e (getL s (third m)) then ceval(Seq c (while e c)) m s
                                            		  else ceval Skip m s

ceval (New d c) m s = let e = deval d (getL s (third m)) in ceval c (first m,
																  	second m,
																  	setL (s,e)(third m)) s
ceval (PCall n ns) m s = let v:vs = mgetE (n:ns) (getL s (third m)) in (snd v) m vs

--Declaration eval
deval :: Decl -> ProgEnv -> ProgEnv
deval (DVar n e) env = [(n,eeval e env)]

deval (DSeq d0 d1) env = let x = deval d0 env in x ++ deval d1 (msetE x env)
deval (DProc n ns c) env = [(n,proc)] --proc is a function to be called when calling this procedure
                         where proc m vs = ceval c m (msetE (zip ns vs) env)



