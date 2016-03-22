module Eval where

import Mu
import OBDD
import Types
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

--eval

check :: Form -> Env ->  OBDD AP -> Assoc -> OBDD AP
check (Prop p) v m e = OBDD.unit p True
check (Var n) v m e = e n
check (Not f) v m e = OBDD.not (check f v m e) 
check (And f0 f1) v m e = OBDD.and[check f0 v m e,check f1 v m e]
check (Or f0 f1) v m e = OBDD.or[check f0 v m e,check f1 v m e]
check (Diamond f) v m e = exists_many (Set.fromList [(fst x++"'") | x <- v]) (OBDD.and[m,check2 f v m e])
check (Box f) v m e = check (Not (Diamond (Not f))) v m e
check (Lfp n f) v m e =  fix n f m e (OBDD.constant False) (OBDD.constant True)
						where fix n f m e new old = if OBDD.satisfiable (OBDD.and[OBDD.or[new,OBDD.not old],OBDD.or[old,OBDD.not new]]) then new 
								   				   else fix n f m (update (n,new) e) (check f v m (update (n,new) e)) new               
check (Gfp n f) v m e =  fix n f m e (check f v m e) (OBDD.constant True)
					   where fix n f m e new old = if OBDD.satisfiable (OBDD.and[OBDD.or[new,OBDD.not old],OBDD.or[old,OBDD.not new]]) then new 
								   				   else fix n f m (update (n,new) e) (check f v m (update (n,new) e)) new


check2 :: Form -> Env -> OBDD AP -> Assoc -> OBDD AP
check2 (Prop p) v m e = OBDD.unit (p++"'") True
check2 (Var n) v m e = e n
check2 (Not f) v m e = OBDD.not (check2 f v m e) 
check2 (And f0 f1) v m e = OBDD.and[check2 f0 v m e,check2 f1 v m e]
check2 (Or f0 f1) v m e = OBDD.or[check2 f0 v m e,check2 f1 v m e]
check2 (Diamond f) v m e = exists_many (Set.fromList [(fst x++"'") | x <- v]) (OBDD.and[m,check2 f v m e])
check2 (Box f) v m e = check2 (Not (Diamond (Not f))) v m e
check2 (Lfp n f) v m e =  fix n f m e (OBDD.constant False) (OBDD.constant True)
						where fix n f m e new old = if OBDD.satisfiable (OBDD.and[OBDD.or[new,OBDD.not old],OBDD.or[old,OBDD.not new]]) then new 
								   				   else fix n f m (update (n,new) e) (check2 f v m (update (n,new) e)) new              
check2 (Gfp n f) v m e =  fix n f m e (check2 f v m e) (OBDD.constant True)
					   where fix n f m e new old = if OBDD.satisfiable (OBDD.and[OBDD.or[new,OBDD.not old],OBDD.or[old,OBDD.not new]]) then new 
								   				   else fix n f m (update (n,new) e) (check2 f v m (update (n,new) e)) new

