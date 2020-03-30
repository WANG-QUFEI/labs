module Exception
  (
    module Control.Exception
  , CompilerException(..)
  , throwTce
  ) where

import Control.Exception

data CompilerException = TypeCheckException String

instance Show CompilerException where
  show (TypeCheckException str) = str

instance Exception CompilerException

throwTce :: String -> a
throwTce str = (throw . TypeCheckException) str
