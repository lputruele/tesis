module Types where

import Data.List
import ParseLib
import OBDD

type Name = String
type AP = String
type Env = [(AP,Bool)]
type Assoc = Name -> OBDD AP

assoc0 :: Assoc
assoc0 = \n -> OBDD.constant False

update :: (Name,OBDD AP) -> Assoc -> Assoc
update (n,v) a = \m -> if m == n then v else a m 

