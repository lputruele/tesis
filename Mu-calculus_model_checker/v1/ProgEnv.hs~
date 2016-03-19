module ProgEnv where

import Types

ProgEnv0 :: ProgEnv
ProgEnv0 = []

setE :: (Name,Value) -> ProgEnv -> ProgEnv
setE (n,v) [] = (n,v)
setE (n,v) (x:xs) = if fst x == n then (n,v) : xs 
                  else x : setE (n,v) xs 

getE :: Name -> ProgEnv -> Value
getE n [] = False 
getE n (x:xs) = if fst x == n then snd x
              else getE n xs

msetE :: ProgEnv-> ProgEnv -> ProgEnv
msetE (x:xs) ProgEnv = msetE xs (setE x ProgEnv)

mgetE :: [Name] -> ProgEnv -> ProgEnv
mgetE (n:ns) ProgEnv = (n,getE n ProgEnv) : mgetE ns ProgEnv

iniE :: ProgEnv -> ProgEnv
iniE nds = msetE nds ProgEnv0

