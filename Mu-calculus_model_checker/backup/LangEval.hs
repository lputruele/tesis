module LangEval where

import Types
import Lang
import Env
import OBDD
import System.Random

--Expression eval
eeval :: Expr -> Env -> Value
eeval (Bin op e0 e1) env | op `elem` [And, Or, Xor, Impl, Iff] = mkBValue (bvalue (eeval e0)) `operate` (bvalue(eeval e1))
                                                               where operate = opBool op
eeval (Not e) env = mkBValue not (bvalue(eeval e))
eeval (Lit v) env = v
eeval (Ident n) env = getE n env

--Expression operators
opBool :: Op -> BValue -> BValue -> BValue
opBool And = (&)
opBool Or = (|)
opBool Xor = (/)
opBool Impl = (->)
opBool Iff = (<->)

--Auxiliary functions
evalGuards :: (Expr,Comm) -> OBDD AP -> Env -> (OBDD AP,Env)
evalGuards [] o e = (o,e) 
evalGuards (z:zs) o e = if (i==0) then ceval (snd z) o e
                                         else evalGuards zs o e

--Command eval
--takes a Lang program and makes an OBDD out of it
ceval :: Comm -> OBDD AP -> Env -> (OBDD AP,Env)
ceval Skip obdd env = (obdd,env) 
ceval (Assign n e) obdd env = let x = setE (n,eeval e) env in (OBDD.or [obdd,OBDD.and[mkState env, mkState2 x]]) x 
ceval (Seq c0 c1) obdd env = let (x,y) = ceval c0 obdd env in ceval c1 x y
--A guard its composed of a condition and a command
ceval (If []) obdd env = (obdd,env) 
ceval (If x:xs) obdd env = ceval (If xs) (OBDD.or [obdd,OBDD.and[ceval (snd x) obdd env, mkState fst x]]) env                 
                                                                                

ceval (While e c) obdd env = if eeval e env then ceval(Seq c (while e c)) obdd env
                                            else ceval Skip obdd env

ceval (New d c) obdd env = let e = deval d env in ceval c (OBDD.or [obdd, (mkState e)]) (msetE e env)
ceval (PCall n ns) obdd env = let v:vs = mgetE (n:ns) env
                         in (snd v) obdd vs

--Declaration eval
deval :: Decl -> Env -> Env
deval (DVar n e) env = [(n,eeval e env)]

deval (DSeq d0 d1) env = let x = deval d0 env in x ++ deval d1 (msetE x env)
deval (DProc n ns c) env = [(n,proc)] --proc is a function to be called when calling this procedure
                         where proc obdd vs = ceval c obdd (msetE (zip ns vs) env)



