module Main where

import qualified Data.Map as Map
import System.Exit (exitSuccess, exitFailure)

import SReplay

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then exitSuccess
    else exitFailure

type Program =  Replay String String Int

type Result  = ((Int, Int), Map.Map Int (Int, Int))
type Input   = [String]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> IO Result
runProgram prog inp = play emptyTrace inp
  where
    play t inp = do
      r <- runt' prog t
      case r of
        Right (x, t1, t2) -> case inp of
          [] -> let lenOfItems = length . items $ t1
                    transTag   = Map.map (\(step, str) -> (step, read str)) (tags t1)
                in return ((x, lenOfItems), transTag)
          _ -> error "too many inputs"
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r p) = do
  putStrLn $ name ++ ": "
  r' <- runProgram p i
  if r == r'
    then putStrLn "ok" >> return True
    else putStrLn ("FAIL: expected " ++ show r ++
                  " instead of " ++ show r')
         >> return False


-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "test-without-using-cut"
    , testInput   = ["", "", "", ""]
    , testResult  = ((1, 10), Map.fromList [])
    , testProgram = do
        part1
        r2 <- part2
        part3
        return r2
    }
  , TestCase
    { testName    = "test-using-cut"
    , testInput   = ["", "", "", ""]
    , testResult  = ((1, 6), Map.fromList [(4, (8, 1))])
    , testProgram = do
        part1
        r2 <- cut part2
        part3
        return r2
    }
  ]

part1 :: Program
part1 = do
  io . putStrLn $ "begin part1"
  ask "question 1 in part 1"
  io . putStrLn $ "end part1"
  return 0

part2 :: Program
part2 = do
  io . putStrLn $ "begin part2"
  ask "question 1 in part 2"
  ask "question 2 in part 2"
  io . putStrLn $ "end part2"
  return 1

part3 :: Program
part3 = do
  io . putStrLn $ "begin part3"
  ask  "question 1 in part 3"
  io . putStrLn $ "end part3"
  return 0


-- | Running all the test cases.
runTests :: IO [Bool]
runTests = mapM checkTestCase testCases
