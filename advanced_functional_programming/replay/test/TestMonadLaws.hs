module Main where

import Data.IORef  (newIORef, readIORef, modifyIORef)
import System.Exit (exitSuccess, exitFailure)
import Data.Time   (getCurrentTime, diffUTCTime)

import SReplay

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then exitSuccess
    else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> Replay () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName     :: String
  , testInput    :: Input
  , testResult   :: Result
  , testProgram1 :: Program
  , testProgram2 :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram p inp = do
    counter <- newIORef 0
    let tick = modifyIORef counter (+1)
    x <- play (p tick) emptyTrace inp
    n <- readIORef counter
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x -> case inp of
                     [] -> return x
                     _  -> error "too many inputs"
        Left (_, t') -> case inp of
                          []       -> error "too few inputs"
                          a : inp' -> play prog (addAnswer t' a) inp'          

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r p1 p2) = do
  putStrLn $ "test [" ++ name ++ "]: "
  r1 <- runProgram p1 i
  r2 <- runProgram p2 i
  if r == r1 && r == r2
    then putStrLn "ok" >> return True
    else putStrLn ("FAIL: expected " ++ show r ++
                  " instead of r1: " ++ show r1 ++ ", r2: " ++ show r2)
         >> return False

f :: IO () -> Int -> Replay () Int Int
f a1 a2 = do
  io a1
  n1  <- ask ()
  n2  <- ask ()
  io a1
  return (2^n1 * 3^n2 + a2)

-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName     = "left identity"
    , testInput    = [59, 65]
    , testResult   = (2^59 * 3^65 + 1, 2)
    , testProgram1 = \tick -> return 1 >>= f tick
    , testProgram2 = \tick -> f tick 1
    }
  , TestCase
    { testName     = "right identity"
    , testInput    = [3, 4]
    , testResult   = (2^3 * 3^4 + 1, 2)
    , testProgram1 = \tick -> f tick 1 >>= return
    , testProgram2 = \tick -> f tick 1
    }
  , TestCase  
    {testName      = "associativity"
    , testInput    = [1, 1]
    , testResult   = (49, 2)
    , testProgram1 = \tick -> return 1 >>= f tick >>= (\x -> return (x^2))
    , testProgram2 = \tick -> return 1 >>= (\x -> f tick x >>= (\y -> return (y^2)))
    }
  ]

-- | Running all the test cases.
runTests :: IO [Bool]
runTests = mapM checkTestCase testCases
