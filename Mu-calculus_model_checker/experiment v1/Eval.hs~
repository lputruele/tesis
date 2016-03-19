module Eval where

import Mu
import Kripke
import Types
import List


box1 :: State -> [(State,State)] -> [State] -> Bool
box1 s [] ss = True
box1 s (x:xs) ss = if s == fst x then snd x `elem` ss && box1 s xs ss
            else  box1 s xs ss

diamond1 :: State -> [(State,State)] -> [State] -> Bool
diamond1 s [] ss = False
diamond1 s (x:xs) ss = if s == fst x then snd x `elem` ss || diamond1 s xs ss
            else  diamond1 s xs ss

--naive eval

neval :: Form -> Kripke -> Env -> [State]
neval (Prop p) m e = [s | s <- first m, p `elem` getL s (third m)]
neval (Var n) m e = e n
neval (Not f) m e = (first m) \\ neval f m e
neval (And f0 f1) m e = neval f0 m e `intersect` neval f1 m e
neval (Or f0 f1) m e = neval f0 m e `union` neval f1 m e
neval (Diamond a f) m e = let n = neval f m e in [s | s <- first m, diamond1 s (second m) n ]
neval (Box a f) m e = let n = neval f m e in [s | s <- first m, box1 s (second m) n]
neval (Mu n f) m e = let fix val old = if val /= old then fix (neval f m (setE (n,val) e)) val
                                     else val in fix [] (first m)                   
neval (Nu n f) m e = fix (first m) []
                   where fix val old = if val /= old then fix (neval f m (setE (n,val) e)) val
                                     else val




