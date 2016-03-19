module Types where

import List
import ParseLib

type Name = String
type Action = String
type AP = (Name,Bool)
type State = Int
type Env = Name -> [State]

type ProgEnv = [AP]
type Labels = State -> ProgEnv
type Transition = (State,State)
type Kripke = ([State],[Transition],Labels)





