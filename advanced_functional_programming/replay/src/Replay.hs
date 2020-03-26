{-|
Module      : Replay
Description : An implementation of the 'Replay' monad
Stability   : experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Replay
  (
    -- * Types
    -- ** A 'replay monad' that uses 'IO' as the underling monad
    Replay
    -- ** 'replay monad' transformer
  , ReplayT(..)
  , Item(..)
  , Trace
    -- * Primitive operations
  , iot
  , askt
  , runt
  , emptyTrace
  , addAnswer
  , liftR
    -- * Derived operations
  , io
  , ask
  , run
  ) where

import Control.Applicative
import Control.Monad

-- | The 'replay monad' transformer. It's a function that accepts two 'Trace's, one is the
-- trace accumulated by previous computatiosn, the other is the trace with upcoming Results
-- or Answers. The result of the function is a 'Either' data type within the underlying
-- monad.
newtype ReplayT m q r a = ReplayT {replayT :: Trace r -> Trace r -> m (Either (q, Trace r) (a, Trace r, Trace r))} 

-- | Make 'ReplayT' an instance of 'Monad'
instance Monad m => Monad (ReplayT m q r) where
  return = return'
  (>>=)  = bind

-- | Make 'ReplayT' an instance of 'Applicative'
instance Monad m => Applicative (ReplayT m q r) where
    pure = return
    (<*>) = ap

-- | Make 'ReplayT' an instance of 'Functor'
instance Monad m => Functor (ReplayT m q r) where
    fmap = liftM

-- | For lifting computations from the underlying monad
liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR m = ReplayT $ \before after -> do a <- m
                                        return $ Right (a, before, after)

-- | The item of a Trace, could be an answer to a question or a string result of an io
-- action
data Item r = Answer r | Result String deriving (Show, Read)

-- | The sequence of results of potentially several computations
type Trace r = [Item r]

-- | Definition of monad 'return' method
return' :: Monad m => a -> ReplayT m q r a
return' a = ReplayT $ \before after -> return $ Right (a, before, after)

-- | Definition of monad '>>=' method
bind :: Monad m => ReplayT m q r a -> (a -> ReplayT m q r b) -> ReplayT m q r b
bind r1 f = ReplayT $ \before after -> do
  a1 <- replayT r1 before after
  case a1 of
    Left l -> return (Left l)
    Right (a, t1, t2) -> replayT (f a) t1 t2

-- | Generalized io method. If the value of the underlying monad already exists in the
-- upcoming trace, then the monad operation is not performed; Otherwise perform the
-- monad computation and return its value as the first part of the new result. Update
-- the traces of the past and furture computations at the same time. 
iot :: (Show a, Read a, MonadFail m) => m a -> ReplayT m q r a
iot m = ReplayT $ \before after -> case after of
                                     [] -> do a <- m
                                              return $ Right (a, before ++ [Result (show a)], [])
                                     (x:xs) -> case x of
                                                 Result s -> return $ Right (read s, before ++ [x], xs)
                                                 _        -> fail "type mismatch, expect: Result String, actual: Answer r"

-- | Generalized 'ask' method. If an answer to the question aleardy exists in the upcoming
-- trace, return that answer as the result. Otherwise return a 'Left' value consists of
-- the question and the trace of past computations.
askt :: MonadFail m => q -> ReplayT m q r r
askt q = ReplayT $ \before after ->
   case after of
    [] -> return $ Left (q, before)
    (x:xs) -> case x of
                Answer r -> return $ Right (r, before ++ [x], xs)
                _        -> fail "type mismatch, expect: Answer r, actual: Result String"

-- | Generalized 'run' method. Accept a 'replay' monad and a trace, produce a value of the
-- underlying monad
runt :: Monad m => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
runt rt t = let m = replayT rt emptyTrace t in
  do a1 <- m
     case a1 of
       Left l -> return $ Left l
       Right (a2, t1, t2) -> return $ Right a2

emptyTrace :: Trace r
emptyTrace = []

addAnswer :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r]

-- | IO based 'replay' monad
type Replay q r a = ReplayT IO q r a

-- | IO based 'io' operation
io :: (Show a, Read a) => IO a -> Replay q r a
io = iot

-- | IO based 'ask' operation
ask :: q -> Replay q r r
ask = askt

-- | IO based 'run' operation
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = runt



  
