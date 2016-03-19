module Types where

import List
import ParseLib

type Name = String
type Action = String
type AP = String
type State = Int
type Env = Name -> [State]
type Labels = State -> [AP]
type Transition = (State,State)
type Kripke = ([State],[Transition],Labels)

