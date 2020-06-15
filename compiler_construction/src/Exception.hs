module Exception
  (
    module Control.Exception
  , CompilerException(..)
  , throwTce
  , throwCge
  ) where

import Control.Exception
import Data.Char

data CompilerException = TypeCheckException String | LLvmGenerateException String

instance Show CompilerException where
  show (TypeCheckException str) = errorMessage "type check exception" str
  show (LLvmGenerateException str) = errorMessage "code generate exception" str

instance Exception CompilerException

throwTce :: String -> a
throwTce str = (throw . TypeCheckException) str

throwCge :: String -> a
throwCge str = (throw . LLvmGenerateException) str

errorMessage :: String -> String -> String
errorMessage title body =
  let n = 25
      s1 = concat $ replicate n "."
      s2 = fmap toUpper title
      l  = 2 * n + (length s2) + 4
      s3 = replicate l '.'
  in s1 ++ "/ " ++ s2 ++ " \\" ++ s1 ++ "\n" ++ body ++ "\n" ++ s3 ++ "\n"
