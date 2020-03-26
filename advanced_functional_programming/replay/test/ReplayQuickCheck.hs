{-|
Module      : Main
Description : A test module using 'QuickCheck' to generate random test cases to verify
              that our monad transformer 'ReplayT m q r a' abides by the monadic laws 
Stability   : experimental
-}
module Main where

import Test.QuickCheck
import Control.Monad

import Replay


main :: IO ()
main = do
  putStrLn "check left identity"
  quickCheck prop_leftIdentity
  putStrLn "check right identity"
  quickCheck prop_rightIdentity
  putStrLn "check associativity"
  quickCheck prop_associativity
  return ()

-- | a type synonym. We choose 'Maybe' as the underlying monad because even if
-- a random input trace with mismatched types of values presents, 'quickCheck' can
-- still proceeds without a crash by virtue of the implementation of function 'fail'
-- from the 'Maybe' monad, which just returns 'Nothing'.
type ReplayTest = ReplayT Maybe String String Int

-- | a type synonym for the binding function
newtype BindF = BindF {ff :: Int -> ReplayTest}

-- | only to confirm with the interface specification of function 'forAll'
instance Show (ReplayT m q r a) where
  show (ReplayT f) = "some replayT value"

-- | only to confirm with the interface specification of function 'forAll'
instance Show BindF where
  show _ = "some binding function"

-- | equality of type Item
instance Eq a => Eq (Item a) where  
    Answer x == Answer y = x == y  
    Result x == Result y = x == y 
    _ == _ = False

-- | a factory method with accepts an Int and produces a ReplayTest
replayFactory :: Int -> ReplayTest
replayFactory i = do
  iot $ Just ("iot - maybe - " ++ show i)
  askt ("askt - maybe - " ++ show i)
  return 0

-- | use a random list of Int to compose a random ReplayTest
genReplay :: Gen ReplayTest
genReplay = do
  l <- genIntList
  let r = f l in return r
  where f = foldl (\r i -> r >> replayFactory i) (return 0)

-- | generate a random list of Int
genIntList :: Gen [Int]
genIntList = do
  num <- choose (0, 1000)
  replicateM num (choose (0, 1000))

-- | generate a random Trace value
genTrace :: Gen (Trace String)
genTrace = do
  num <- choose (0, 1000)
  replicateM num genItem

-- | generate a random Item value
genItem :: Gen (Item String)
genItem = do
  i <- choose (0, 1) :: Gen Int
  if i == 0 then return $ Answer "" else return $ Result ""

-- | generate a random binding function
genFun :: Gen BindF
genFun = do
  n  <- choose (0, 1000)
  rt <- genReplay
  return $ BindF $ \i -> do a <- rt
                            return (i + a - n)
-- | random test cases for left identity
prop_leftIdentity x = forAll genReplay $ \replay ->
    forAll genTrace $ \trace -> forAll genFun $ \bindf ->
                                                  let f  = ff bindf
                                                      m1 = return x >>= f
                                                      m2 = f x
                                                  in runt m1 trace == runt m2 trace
  where types = x :: Int

-- | random test cases for right identity
prop_rightIdentity = forAll genReplay $ \replay ->
    forAll genTrace $ \trace -> let m1 = replay >>= return
                                    m2 = replay
                                in runt m1 trace == runt m2 trace

-- | random test cases for associativity
prop_associativity = forAll genReplay $ \replay ->
    forAll genTrace $ \trace ->
      forAll genFun $ \bindf1 ->
        forAll genFun $ \bindf2 -> let k = ff bindf1
                                       h = ff bindf2
                                       m1 = replay >>= k >>= h
                                       m2 = replay >>= (\x -> k x >>= h)
                                   in runt m1 trace == runt m2 trace
