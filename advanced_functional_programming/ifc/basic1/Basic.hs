{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Version 2 of the interpreter
module Basic where

import Control.Monad.Identity
import Control.Monad.Reader
import           Data.Map        (Map)
import qualified Data.Map as Map
import qualified Expr_Parser as P (parseExpr, Language (..))

-- | data type which is used to label 'secret' or 'public' values
data Sensitivity = Secret | Public deriving (Show, Eq)

-- | Sensitivity is a Monoid
instance Monoid Sensitivity where
  mempty = Public
  mappend Public Public = Public
  mappend _ _ = Secret


-- | Sensitivity is a Semigroup
instance Semigroup Sensitivity where
  (<>) = mappend

-- | labeled value
data Labeled a = LV Sensitivity a deriving (Show, Eq)

-- | expression definition
data Expr = Lit Integer
          | Expr :+: Expr
          | Var Name
          | Let Name Expr Expr
          | Sec Expr
  deriving (Show)

-- | Preliminaries for (immutable) local bindings
type Name  = String
type Value = Integer

-- | An environment maps variables to labeled values.
type Env = Map Name (Labeled Value)

emptyEnv :: Env
emptyEnv = Map.empty

newtype Eval a = MkEval (ReaderT Env Identity a)
  deriving (Functor, Applicative, Monad, MonadReader Env)

runEval :: Eval a -> a
runEval (MkEval rd) = runIdentity (runReaderT rd emptyEnv)

-- | Environment manipulation
lookupVar :: Name -> Eval (Labeled Value)
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> fail $ "Variable " ++ x ++ " not found."
    Just v  -> return v

-- | update variable binding
localScope :: Name -> Labeled Value -> Eval a -> Eval a
localScope n v = local (Map.insert n v)

-- | evaluate an expression
eval :: Expr -> Eval (Labeled Value)
eval (Lit n)       = return (LV Public n)
eval (a :+: b)     = do
  LV s1 v1 <- eval a
  LV s2 v2 <- eval b
  return $ LV (s1 `mappend` s2) (v1 + v2)
eval (Var x)       = lookupVar x
eval (Let n e1 e2) = do v <- eval e1
                        localScope n v (eval e2)
eval (Sec e)       = do LV _ v <- eval e
                        return $ LV Secret v

-- * Utilities: testing and parsing

-- * Test for variables declaration
test1, test2, test3, test4 :: Expr
runtest1, runtest2, runtest3, runtest4 :: Labeled Value

test1    = parse "let x=10; x"
runtest1 = runEval $ eval test1

test2    = parse "let x=secret 10; x"
runtest2 = runEval $ eval test2

test3    = parse "let x=secret 10; let y = 20; x+y"
runtest3 = runEval $ eval test3

test4    = parse "let x=10; let y=20; x+y"
runtest4 = runEval $ eval test4

testCases = [test1, test2, test3, test4]
expectedResults = [LV Public 10,LV Secret 10,LV Secret 30,LV Public 30]

testRunner testCase expectedResult = runEval (eval testCase) == expectedResult

testLabeledValue = (and $ zipWith testRunner testCases expectedResults) == True

-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewSecretexp = Sec
  , P.lNewSecretref = error "Not yet defined"
  , P.lNewref = error "Not yet defined"
  , P.lDeref  = error "Not yet defined"
  , P.lAssign = error "Not yet defined"
  , P.lCatch  = error "Not yet defined"
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
