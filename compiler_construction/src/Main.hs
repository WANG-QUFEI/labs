module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import qualified Data.Text as T

import LexJavalette
import ParJavalette
import PrintJavalette
import AbsJavalette
import ErrM
import TypeChecker
import CodeGen
import Exception

myLLexer = myLexer

runFile :: Int -> Maybe FilePath -> FilePath -> IO ()
runFile level m_outputFile inputFile = do
  src <- readFile inputFile
  run level m_outputFile src

run :: Int -> Maybe FilePath -> String -> IO ()
run level m_outputFile src = do
  let ts = myLLexer src
      ps = pProg ts
  case ps of
    Bad s -> do
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr "\n**** Parse error ****\n"
      hPutStrLn stderr "Tokens:"
      hPutStrLn stderr $ show ts
      putStrLn s
      exitFailure
    Ok prog -> case level of
      0 -> do
        hPutStrLn stderr "OK"
        showTree prog
      _ -> do
        env <- catch (runTypeCheck prog) handler
        case level of
          1 -> hPutStrLn stderr "OK"
          _ -> do
            (prog', codeTxt) <- runCodeGenerate prog env
            hPutStrLn stderr "OK"
            let codeString = T.unpack codeTxt
            putStrLn codeString
            case level of
              2 -> return ()
              _ -> case m_outputFile of
                Just filePath -> writeFile filePath codeString
  exitSuccess   
  where
    handler e = do
      hPutStrLn stderr "ERROR"
      hPutStrLn stderr (show (e :: CompilerException))
      exitFailure

showTree :: (Show a, Print a) => a -> IO ()
showTree tree = do
  hPutStrLn stderr $ "[Abstract Syntax]\n\n" ++ show tree
  hPutStrLn stderr $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse, type check and generate code from stdin."
    , "  (files)         Parse, type check and generate code from files."
    , "  -p (files)      Only do parsing of files."
    , "  -t (files)      Only do type checking of files."
    , "  -w (dst_file) (files) Parse, type check and generate code for 'files', write the output to 'dst_file' simultaneously"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 Nothing
    "-p" : fs -> mapM_ (runFile 0 Nothing) fs
    "-t" : fs -> mapM_ (runFile 1 Nothing) fs
    "-w" : xs -> case xs of
      path : fs -> mapM_ (runFile 3 (Just path)) fs
    fs -> mapM_ (runFile 2 Nothing) fs
