module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexJavalette
import ParJavalette
import PrintJavalette
import AbsJavalette
import ErrM
import TypeChecker
import Exception

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> FilePath -> IO ()
runFile v f = do
  s <- readFile f
  run v s

run :: Verbosity -> String -> IO ()
run v s = let ts = myLLexer s in case pProg ts of
  Bad s -> do
    hPutStrLn stderr "ERROR"
    putStrLn "\nParse      Failed...\n"
    putStrV v "Tokens:"
    putStrV v $ show ts
    putStrLn s
    exitFailure
  Ok prog -> do
    catch (runTypeCheck prog) h
    hPutStrLn stderr "OK"
    putStrLn "\nParse Successful and Type Checked!"
    showTree v prog   
  where h e = do
          hPutStrLn stderr "ERROR\n"
          hPutStrLn stderr (show (e :: CompilerException))
          exitFailure

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2
    "-s" : fs -> mapM_ (runFile 0) fs
    fs -> mapM_ (runFile 2) fs
