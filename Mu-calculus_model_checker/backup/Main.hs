#!/usr/bin/runhugs

module Main where

import ParseLib
import Mu
import List
import Types
import LangSyntax
import LangParser
import LangEval
import System
import Eval
import OBDD

pProg :: Parser ([Form],OBDD AP)
pProg = do {p <- pNew; symbol "check"; symbol "{" f <- pForms; symbol "}" 
            return (f,ceval p (OBDD.unit "init state" True) env0)}

-- exec
exec :: ([Form], OBDD AP) -> [Form]
exec ([],obdd) = []
exec ((f:fs), obdd) = check f obdd env0 : exec (fs,obdd)

showResult :: [Form] -> IO ()
showResult [] = putStr ("\n")
showResult (f:fs)  = do 
                        putStr ("program satisfies property: (" ++ show f ++ ") \n")
                        showResult fs 

-- Parse and exec from file

nullCheck n l = if null l then return () else putStr n
notnullCheck n l = if null l then putStr n else return ()

ioCheck :: String -> IO ([Form], Kripke)
ioCheck filename = do { src <- readFile filename;
                        ps <- return (papply (parse pProg) src);
                        notnullCheck "Parse error\n" ps;
                        (e,rest) <- return (head ps);
                        nullCheck ("Parse error before :"++rest++"\n") rest;
                        return e
                      }

exec_from_file :: String -> IO () 
exec_from_file filename = do
                             e <- ioCheck ("test/"++filename)
                             showResult (exec e)

-- execute from command line
main :: IO ()
main = do
          a <- getArgs
          if (a == []) then putStr "Usage: ./Main.hs program.\n"
                       else exec_from_file (head a)

