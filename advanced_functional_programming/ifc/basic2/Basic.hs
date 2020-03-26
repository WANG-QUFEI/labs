{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Version 2 of the interpreter
module Basic where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.Map (Map)
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
          | NewRef Expr
          | Deref Expr
          | Expr := Expr
          | Sec Expr
          | Catch Expr Expr
  deriving (Show)

-- | We add an exception type...
data Err = SegmentationFault
         | UnboundVariable String
         | OtherError String
  deriving Show

-- | Preliminaries for (immutable) local bindings
type Name  = String
type Value = Integer

-- | An environment maps variables to labeled values.
type Env = Map Name (Labeled Value)

emptyEnv :: Env
emptyEnv = Map.empty

-- | Preliminaries for mutuable references
type Ptr    = Value
  -- ^ dangerous language: any 'Value' can be used as a 'Ptr'

-- | We need to keep track of the store containing the labeled values of
-- our references. We also remember the next unused pointer, furthermore,
-- we keep track of the stacks of 'try catch' expressions to prevent
-- implicit security leakage
data Store = Store { nextPtr  :: Ptr
                   , heap     :: Map Ptr (Labeled Value)
                   , tryStack :: [Sensitivity]
                   }

emptyStore :: Store
emptyStore = Store 0 Map.empty []

newtype Eval a = MkEval (ExceptT Err (StateT Store (ReaderT Env Identity)) a)
  deriving (Functor, Applicative,
            Monad, MonadState Store, MonadReader Env, MonadError Err)

runEval :: Eval a -> Either Err a
runEval (MkEval err) = runIdentity $ runReaderT
                      (evalStateT
                          (runExceptT err)
                       emptyStore) emptyEnv


-- | Environment manipulation
lookupVar :: Name -> Eval (Labeled Value)
lookupVar x = do
  env <- ask
  case Map.lookup x env of
    Nothing -> throwError (UnboundVariable x)
    Just v  -> return v

-- | update variable binding
localScope :: Name -> Labeled Value -> Eval a -> Eval a
localScope n v = local (Map.insert n v)

-- | Create a new reference containing the given labeled value.
newRef :: Labeled Value -> Eval Ptr
newRef v = do
              store <- get
              let ptr      = nextPtr store
                  ptr'     = 1 + ptr
                  ts       = tryStack store
                  newHeap  = Map.insert ptr v (heap store)
              put (Store ptr' newHeap ts)
              return ptr

-- | Get the labeled value of a reference. Crashes with our own
-- "segfault" if given a non-existing pointer.
deref :: Labeled Ptr -> Eval (Labeled Value)
deref (LV sp p) = do st <- get
                     let h = heap st
                     case Map.lookup p h of
                       Nothing -> throwError SegmentationFault
                       Just (LV sv v)  -> return (LV (sp `mappend` sv) v)

-- | Updating the labeled value of a reference. Has no effect if the
-- reference doesn't exist.
(=:) :: MonadState Store m => Ptr -> Labeled Value -> m (Labeled Value)
p =: v = do store <- get
            let heap' = Map.adjust (const v) p (heap store)
            put (store {heap = heap'})
            return v

-- | push a security gurad for a new 'try catch' calculation
pushGuard :: Sensitivity -> Eval ()
pushGuard s = modify $ \store ->
  let np = nextPtr store
      hp = heap store
      ts = tryStack store
  in Store np hp (s:ts)

-- | update the current security gurad
updateGuard :: Sensitivity -> Eval ()
updateGuard g = modify $ \store ->
  let np = nextPtr store
      hp = heap store
      ts = tryStack store
  in Store np hp (g:(tail ts))

-- | pop a security guard when leaving a 'try catch' calculation
popGuard :: Eval ()
popGuard = modify $ \store ->
  let np = nextPtr store
      hp = heap store
      ts = tryStack store
  in Store np hp (tail ts)

-- | check if current security guard exists
peepGuard :: Eval (Maybe Sensitivity)
peepGuard = do
  s <- get
  let ts = tryStack s
  case ts of
    [] -> return Nothing
    _  -> return . Just $ head ts

-- | check and update the current security guard
peepAndUpdate :: Sensitivity -> Eval ()
peepAndUpdate s = do
  mg <- peepGuard
  case mg of
    Nothing -> return ()
    Just g  -> updateGuard (s `mappend` g)

-- | evaluate an expression
eval :: Expr -> Eval (Labeled Value)
eval (Lit n)       = return (LV Public n)
eval (a :+: b)     = do
  LV s1 v1 <- eval a
  LV s2 v2 <- eval b
  return $ LV (s1 `mappend` s2) (v1 + v2)
eval (Var x)       = lookupVar x
eval (Let n e1 e2) = do LV s v <- eval e1
                        localScope n (LV s v) (eval e2)
eval (Sec e)       = do LV _ v <- eval e
                        return $ LV Secret v
eval (NewRef e)    = do LV s v <- eval e
                        ptr <- newRef (LV s v)
                        return $ LV s ptr
eval (Deref e)     = do LV s p <- eval e
                        peepAndUpdate s
                        deref (LV s p)                        
eval (pe := ve)    = do LV sp p <- eval pe
                        LV sv v <- eval ve
                        mg <- peepGuard
                        if sp == Public && (sv == Secret || mg == Just Secret)
                          then fail "security violation"
                          else p =: LV (sp `mappend` sv) v
eval (Catch e1 e2) = do
  pushGuard Public
  a <- catchError (eval e1) (\err -> f e2)
  popGuard
  return a
  where f e = do
          LV s v <- eval e2
          mg <- peepGuard
          case mg of
            Nothing -> return (LV s v)
            Just s' -> return (LV (s `mappend` s') v)

runWithInput :: Eval a -> IO (Either Err a)
runWithInput x = do
  putStr "Enter the value of secret variable input: "
  str <- getLine
  let p  = read str :: Ptr
      x' = localScope "input" (LV Secret p) x
  return $ runEval x'

-- * Utilities: testing and parsing

-- Test for variables declaration
test1, test2, test3, test4 :: Expr
runtest1, runtest2, runtest3, runtest4 :: Either Err (Labeled Value)

test1    = parse "let x=10; x"
runtest1 = runEval $ eval test1

test2    = parse "let x=secret 10; x"
runtest2 = runEval $ eval test2

test3    = parse "let x=secret 10; let y=20; x+y"
runtest3 = runEval $ eval test3

test4    = parse "let x=10; let y=20; x+y"
runtest4 = runEval $ eval test4

-- Test for references
test5, test6, test7, test8, test9, test10, testInput, attack, attack2 :: Expr
runtest5, runtest6, runtest7, runtest8, runtest9, runtest10 :: Either Err (Labeled Value)

test5    = parse "let p=new 1; !p"
runtest5 = runEval $ eval test5

test6    = parse "let p=secret 42; p"
runtest6 = runEval $ eval test6

test7    = parse "let s=new (secret 42); let p=new 10; !s+!p"
runtest7 = runEval $ eval test7

-- It should fail!
test8    = parse "let s=secret 42; let p=new 10; p := s"
runtest8 = runEval $ eval test8

-- It should succeed!
test9    = parse "let s=new (secret 42); let p=new 10; s := !p ; !s"
runtest9 = runEval $ eval test9

-- It should fail!
test10 = parse "let s=new (secret 42); let p=new 10; p := !s"
runtest10 = runEval $ eval test10

testInput = parse "let p1=new 42; let p2=new 100; !(p1+input)"


attack = parse "let ref=new 0; let leak = (try !(ref+input) catch (ref := 1)); !ref"

attack2 = parse "let ref = new 0; try (!(ref+input); ref := 2) catch 2"
-- | The parser is parameterised over the abstract syntax.
language :: P.Language Expr
language = P.Lang
  { P.lLit    = Lit
  , P.lPlus   = (:+:)
  , P.lLet    = Let
  , P.lVar    = Var
  , P.lNewref = NewRef
  , P.lNewSecretref = Sec
  , P.lDeref  = Deref
  , P.lAssign = (:=)
  , P.lCatch  = Catch
  }

parse :: String -> Expr
parse s = case P.parseExpr language s of
  Left err -> error (show err)
  Right x  -> x
