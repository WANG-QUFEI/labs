{-# LANGUAGE OverloadedStrings #-}
module Main where

import ReplayWeb
import Web.Scotty (ActionM, scotty, get, post)

main :: IO ()
main = scotty 3000 $ do
    get "/" serve
    post "/" serve
  where
    serve :: ActionM ()
    serve = runWeb $ prog
    prog  = do
      cut . io $ putStrLn "[info] About to run questions in form-1"
      ans1 <- cut $ ask form1
      cut . io $ putStrLn "[info] About to run questions in form-2"
      ans2 <- cut $ ask form2  
      cut . io $ putStrLn "[info] About to return the result" 
      return $ toPersonRecord ans1 ans2
    form1 :: Form
    form1 = [(Q "What's your name?" 0 [checkIfEmpty, checkLength]),
             (Q "What's your favourite integer?" 1 [checkIfInteger, checkLength]),
             (Q "What's your favourite programming language?" 2 [checkIfEmpty, checkLength])]
    form2 :: Form
    form2 = [(Q "What's the name of your pet?" 3 [checkIfEmpty, checkLength]),
             (Q "What's your second favourite integer?" 4 [checkIfInteger, checkLength]),
             (Q "Do you enjoying this programme?" 5 [checkAffirmative])]
    toPersonRecord :: Answers -> Answers -> PersonRecord
    toPersonRecord a1 a2 = PR (takeAnswer a1 0) (read $ takeAnswer a1 1) (takeAnswer a1 2)
                              (takeAnswer a2 0) (read $ takeAnswer a2 1)
    takeAnswer :: Answers -> Int -> String
    takeAnswer a i = snd $ a !! i
