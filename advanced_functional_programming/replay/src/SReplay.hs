{-|
Module      : SReplay
Description : An implementation of the 'State Replay' monad
Stability   : experimental
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SReplay
  (
    -- * Types
    -- ** A 'replay monad' that uses 'IO' as the underling monad
    Replay
    -- ** 'replay monad' transformer
  , ReplayT(..)
  , Item(..)
  , Trace(..)
    -- * Primitive operations
  , iot
  , askt
  , runt
  , emptyTrace
  , addAnswer
  , liftR
  , cut
  , runt'
    -- * Derived operations
  , io
  , ask
  , run
  ) where

import Control.Applicative
import Control.Monad
import Data.Map as Map

-- | The 'replay monad' transformer. It's a function that accepts a step value and two 'Trace's, one is the
-- trace accumulated by previous computatiosn, the other is the trace with upcoming Results
-- or Answers. The result of the function is a tuple within the underlying monad, with the first part being a 'Either' data type
-- and the second part being the current step or state it proceeds into.
newtype ReplayT m q r a = ReplayT {replayT :: Step -> Trace r -> Trace r -> m (Either (q, Trace r) (a, Trace r, Trace r), Step)}

-- | IO based 'replay' monad
type Replay q r a = ReplayT IO q r a

-- | State of the monad, used to identity the current step of computation.
type Step = Int

-- | The item of a Trace, could be an answer to a question or a string result of an io
-- action
data Item r = Answer r | Result String deriving (Show, Read)

-- | A stated trace, with 'items' being the answers or results in the trace, 'tags' being the recorder of the ranges of computations that
-- have been done before and 'step' being a indicator of the current step of computation
data Trace r = Trace {items :: [(Step, Item r)], tags :: Map Step (Step, String), step :: Step} deriving (Show, Read)

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

-- | create a tag for a range of computations that can be 'cut'
createTag :: Step -> Step -> String -> Trace r -> Trace r
createTag s1 s2 str tr = tr {tags = Map.insert s1 (s2, str) (tags tr)}

-- | delete a tag for a range of computations
deleteTag :: Step -> Trace r -> Trace r
deleteTag s1 tr = tr {tags = Map.delete s1 (tags tr)}

-- | transform a tag from one pair of traces to another
shiftTag :: Step -> Step -> String -> (Trace r, Trace r) -> (Trace r, Trace r)
shiftTag s1 s2 str (tr1, tr2) = (tr1', tr2')
  where tr1' = createTag s1 s2 str tr1
        tr2' = deleteTag s1 tr2

-- | add items(Answers or Results) to a trace
addItems :: Trace r -> [(Step, Item r)] -> Trace r
addItems tr [] = tr
addItems tr is = tr {items = items tr ++ is}

-- | update a trace with the provided items
updateItems :: Trace r -> [(Step, Item r)] -> Trace r
updateItems tr is = tr {items = is}

-- | increment the step of a trace
stepIncr :: Trace r -> Trace r
stepIncr tr = tr {step = (step tr) + 1}

-- | update the step of a trace
updateStep :: Step -> Trace r -> Trace r
updateStep s tr = tr {step = s}

-- | reset the step of a trace to 0, usually used when the trace is provided as an input at the beginning of a computation
resetStep :: Trace r -> Trace r
resetStep tr = tr {step = 0}

-- | For lifting computations from the underlying monad
liftR :: (Monad m, Show a, Read a) => m a -> ReplayT m q r a
liftR m = ReplayT $ \step before after -> do a <- m
                                             return $ (Right (a, before, after), step)

-- | Definition of monad 'return' method
return' :: Monad m => a -> ReplayT m q r a
return' a = ReplayT $ \step before after -> return $ (Right (a, before, after), step)

-- | Definition of monad '>>=' method
bind :: Monad m => ReplayT m q r a -> (a -> ReplayT m q r b) -> ReplayT m q r b
bind r1 f = ReplayT $ \step before after -> do
  (a1, step') <- replayT r1 step before after
  case a1 of
    Left l -> return (Left l, step')
    Right (a, t1, t2) -> replayT (f a) (step' + 1) (stepIncr t1) (stepIncr t2)

-- | Generalized io method. If the value of the underlying monad already exists in the
-- upcoming trace, then the monad operation is not performed; Otherwise perform the
-- monad computation and return its value as the first part of the new result. Update
-- the traces of the past and furture computations at the same time. 
iot :: (Show a, Read a, Monad m) => m a -> ReplayT m q r a
iot m = ReplayT $ \step before after -> case (items after) of
                                        [] -> do a <- m
                                                 return $ (Right (a, addItems before [(step, Result $ show a)], after), step)
                                        (x:xs) -> case x of
                                                   (_ , Result str) -> return $ (Right (read str, addItems before [(step, Result str)], updateItems after xs), step)
                                                   _ -> error "type mismatch, expect: Result String, actual: Answer r"

-- | Generalized 'ask' method. If an answer to the question aleardy exists in the upcoming
-- trace, return that answer as the result. Otherwise return a 'Left' value consists of
-- the question and the trace of past computations.
askt :: Monad m => q -> ReplayT m q r r
askt q = ReplayT $ \step before after ->
  case (items after) of
    [] -> return $ (Left (q, before), step)
    (x:xs) -> case x of
                (_, Answer r) -> return $ (Right (r, addItems before [(step, Answer r)], updateItems after xs), step)
                _ -> error "type mismatch, expect: Answer r, actual: Result String"


-- | cut the traces of already finished computations
cut :: (Monad m, Read a, Show a) => ReplayT m q r a -> ReplayT m q r a
cut rt = ReplayT $ \step before after -> do
  let tag = Map.lookup step (tags after) -- ^ look up for the tag to see if the current computation can be cut
  case tag of
    Just (step', str) -> let (before', after') = shiftTag step step' str (before, after) -- ^ a tag is found, shift the tag from 'after' to 'before' so that we don't lose it.
                             l = dropWhile ((<= step') . fst) (items after)
                         in return (Right (read str, updateStep step' before', updateStep step' (updateItems after' l)), step') -- ^ update the state of traces and return the recorded
                                                                                                                                -- ^ result
    Nothing -> do -- ^ no tag is found, we run it as usual
      (ei, step') <- replayT rt step before after
      case ei of
        Right (a, t1, t2) -> return (Right (a, createTag step step' (show a) t1, t2), step') -- ^ if it finishes with a result, we create a tag for this range of computations
        _ -> return (ei, step') -- ^ otherwise, we return the question and traces as usual
  


-- | Generalized 'run' method. Accept a 'replay' monad and a trace, produce a value of the
-- underlying monad
runt :: Monad m => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
runt rt t = do
  (a1, s) <- replayT rt 0 emptyTrace (resetStep t)
  case a1 of
    Left l -> return $ Left l
    Right (a2, t1, t2) -> return $ Right a2

-- | used for testing, exposing the inner traces
runt' :: Monad m => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) (a, Trace r, Trace r))
runt' rt t = do
  (r, _) <- replayT rt 0 emptyTrace (resetStep t)
  return r


emptyTrace :: Trace r
emptyTrace = Trace [] Map.empty 0

addAnswer :: Trace r -> r -> Trace r
addAnswer tr r = addItems tr [(step tr, Answer r)]

-- | IO based 'io' operation
io :: (Show a, Read a) => IO a -> Replay q r a
io = iot

-- | IO based 'ask' operation
ask :: q -> Replay q r r
ask = askt

-- | IO based 'run' operation
run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)
run = runt
