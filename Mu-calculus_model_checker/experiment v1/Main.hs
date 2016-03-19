#!/usr/bin/runhugs

module Main where

import ParseLib
import Mu
import List
import Types
import Kripke
import System
import Eval
import LangParser

pProg :: Parser ([Form],Kripke)
pProg = do {symbol "PROGRAM";k <- pComm; symbol "CHECK"; f <- pForms; return (f,fst (ceval k ([],[],lbls0) 0))}

-- exec
exec :: ([Form], Kripke) -> [Bool]
exec ([],k) = []
exec ((f:fs), k) = if 0 `elem` (neval f k env0) then True : exec (fs,k)
												else False : exec (fs,k)

showResult :: [Form] -> [Bool] -> IO ()
showResult [] [] = putStr ("\n")
showResult (f:fs) (s:ss) = do 
                              putStr ("(" ++ show f ++ ") ")
                              putStr ("is true in s0" ++ "\n")
                              showResult fs ss

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
                             showResult (fst e) (exec e)

-- execute from command line
main :: IO ()
main = do
          a <- getArgs
          if (a == []) then putStr "Usage: ./Main.hs program.\n"
                       else exec_from_file (head a)

