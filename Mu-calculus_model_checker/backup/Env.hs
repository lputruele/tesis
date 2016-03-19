module Env where

import Types

env0 :: Env
env0 = []

setE :: (Name,Value) -> Env -> Env
setE (n,v) [] = (n,v)
setE (n,v) (x:xs) = if fst x == n then (n,v) : xs 
                  else x : setE (n,v) xs 

getE :: Name -> Env -> Value
getE n [] = mkBValue False 
getE n (x:xs) = if fst x == n then snd x
              else getE n xs

msetE :: Env-> Env -> Env
msetE (x:xs) env = msetE xs (setE x env)

mgetE :: [Name] -> Env -> Env
mgetE (n:ns) env = (n,getE n env) : mgetE ns env

iniE :: Env -> Env
iniE nds = msetE nds env0

