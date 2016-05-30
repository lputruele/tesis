#!/usr/bin/runghc
module Main where

import Data.List
import System.IO
import System.Environment
import OBDD
import OBDD.Data
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M
import ParseLib
import Mu
import MuEval
import Types
import Model
import ModelEval

--Declaration to list of String
str :: Decl -> [String]
str (DVar p) = [p]
str (DSeq d0 d1) = str d0 ++ str d1 

--Main parser for MC2 description
pProg :: Parser (Env,[Form],OBDD AP)
pProg = do {symbol "vars"; d <- pDecl; symbol "rules"; c <- pComm (str d); symbol "init"; e <- pEnv (str d); 
            symbol "check"; f <- pForms (str d); return (deval d e,f,(ceval c (deval d [])))} 

--Instantiante variables in model
inst :: Env -> OBDD AP -> OBDD AP
inst [] obdd = obdd
inst (x:xs) obdd = inst xs (OBDD.instantiate (fst x)(snd x) obdd)

--Execute
exec :: (Env,[Form], OBDD AP) -> [Form]
exec (v,[],obdd) = []
exec (v,(f:fs), obdd) = if OBDD.null  (OBDD.not(inst v (check f v obdd assoc0 False))) then f : exec (v,fs,obdd) 
                        else exec (v,fs,obdd)

exec2 :: (Env,[Form], OBDD AP) -> [OBDD AP]
exec2 (v,[],obdd) = []
exec2 (v,(f:fs), obdd) = ((check f v obdd assoc0 False)) : exec2 (v,fs,obdd) 

--Print results
showResult :: [Form] -> IO ()
showResult [] = putStr ("\n")
showResult (f:fs)  = do 
                        putStr ("Model satisfies property: (" ++ show f ++ ") \n")
                        showResult fs

showResult2 :: [OBDD AP] -> Int -> IO ()
showResult2 [] n = putStr ("\n")
showResult2 (f:fs) n = do 
                        writeFile ("f" ++ show n ++".dot") $ toDot f
                        showResult2 fs (n+1)

--Parse and exec from file
nullCheck n l = if Data.List.null l then return () else putStr n
notnullCheck n l = if Data.List.null l then putStr n else return ()

ioCheck :: String -> IO (Env,[Form], OBDD AP)
ioCheck filename = do { src <- readFile filename;
                        ps <- return (papply (parse pProg) src);
                        notnullCheck "Syntax error\n" ps;
                        (e,rest) <- return (head ps);
                        nullCheck ("Syntax error before :"++rest++"\n") rest;
                        return e
                      }

exec_from_file :: String -> IO () 
exec_from_file filename = do
                             e <- ioCheck ("test/"++filename)
                             writeFile "ok.dot" $ toDot (third e)
                             --showResult2 (exec2 e) 1
                             showResult (exec e)
                        where third (a,b,c) = c

-- execute from command line
main :: IO ()
main = do
          a <- getArgs
          if (a == []) then putStr "Usage: ./Main.hs program.\n"
                       else exec_from_file (head a)

