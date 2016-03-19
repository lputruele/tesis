module Eval where

import Mu
import OBDD
import Types
import List

first :: (a,b,c) -> a
first (a,b,c) = a
second :: (a,b,c) -> b
second (a,b,c) = b
third :: (a,b,c) -> c
third (a,b,c) = c

box1 :: State -> [(State,State)] -> [State] -> Bool
box1 s [] ss = True
box1 s (x:xs) ss = if s == fst x then snd x `elem` ss && box1 s xs ss
            else  box1 s xs ss

diamond1 :: State -> [(State,State)] -> [State] -> Bool
diamond1 s [] ss = False
diamond1 s (x:xs) ss = if s == fst x then snd x `elem` ss || diamond1 s xs ss
            else  diamond1 s xs ss

--naive eval

check :: Form -> OBDD -> Env -> Bool
check (Prop p) m e = [s | s <- first m, p `elem` getL s (third m)]
check (Var n) m e = e n
check (Not f) m e = (first m) \\ check f m e
check (And f0 f1) m e = check f0 m e `intersect` check f1 m e
check (Or f0 f1) m e = check f0 m e `union` check f1 m e
check (Diamond a f) m e = let n = check f m e in [s | s <- first m, diamond1 s (second m) n ]
check (Box a f) m e = let n = check f m e in [s | s <- first m, box1 s (second m) n]
check (Mu n f) m e = let fix val old = if val /= old then fix (check f m (setE (n,val) e)) val
                                     else val in fix [] (first m)                   
check (Nu n f) m e = fix (first m) []
                   where fix val old = if val /= old then fix (check f m (setE (n,val) e)) val
                                     else val




