module Types where

import List
import ParseLib
import OBDD

type BValue = Bool
type PValue = OBDD AP -> [Value] -> (OBDD AP,Env)
type Name = String
type AP = String
type Env = [(Name,Value)]


data Value = BV BValue | PV PValue deriving (Eq,Show)

bvalue :: Value -> BValue
bvalue (BV b) = b

mkState :: Env -> OBDD AP
mkState [] = []
mkState (x:xs) = OBDD.and[OBDD.unit (fst x) (snd x), mkState xs]

mkState2 :: Env -> OBDD AP
mkState2 [] = []
mkState2 (x:xs) = OBDD.and[OBDD.unit ((fst x) ++ "'") (snd x), mkState2 xs]  

mkBValue :: BValue -> Value
mkBValue b = BV b
