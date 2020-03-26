{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : ReplayWeb
Description : A library for Web form application
Stability   : experimental
-}
module ReplayWeb
  (
    module SReplay
    -- | data types
  , Question(..)
  , Form
  , Answers
  , Web
  , PersonRecord(PR)
    -- | functions
  , runWeb
  , checkIfEmpty
  , checkIfInteger
  , checkLength
  , checkAffirmative
  )
  where

import SReplay
import Control.Monad.IO.Class
import Data.Maybe
import Text.Regex.Posix
import Web.Scotty as S
import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import Codec.Binary.Base64.String
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

-- | A question prepared for the user, with 'desc' being the description of the question
-- and 'qseq' being the sequence of the question, which can be used to generate an unique
-- 'id' attribute for the question when converted to a '<input>' html element
data Question = Q {desc :: String, qseq :: Int, verify :: [String -> Maybe String]}

instance Show Question where
  show (Q d q v) = "Desc: " ++ d ++ ", seq: " ++ show q

-- | messages that need to be displayed to the user as a reminder of the wrong input
type Scold = (Int, String)

-- | Answer of the Question. It consists of the sequence of the corresponding question and
-- the actual answer input by the user
type QAnswer = (Int, String)

-- | A Html 'form' element, which can be seen as a collection of relevant questions. For
-- simplicity we only consider '<input>' regardless of all other expressive form elements
-- from Html5
type Form = [Question]

-- | The answer for a form, which is a collection of 'QAnswer'
type Answers = [QAnswer]

data PersonRecord = PR {
  prName :: String,
  fstInt :: Int,
  favLang :: String,
  prPet  :: String,
  sndInt :: Int } deriving (Show, Read)

-- | The web monad which is used to ask question and gather answers
type Web a = Replay Form Answers a

-- | the process of verifying input data
verifyInput :: [String -> Maybe String] -> String -> Maybe String
verifyInput [] _  = Nothing
verifyInput (x:xs) s = let a = x s in
  case a of
    Nothing -> verifyInput xs s
    _       -> a

-- | verify that an answer is not empty
checkIfEmpty :: String -> Maybe String
checkIfEmpty s = let b = isEmptyString s in
  if b then Just "Please fill in this field!" else Nothing

-- | verify that an answer is an integer
checkIfInteger :: String -> Maybe String
checkIfInteger s = let b = ((s =~ ("^-?[1-9][0-9]*$" :: String)) :: Bool) in
  if not b then Just "Please enter an integer!" else Nothing

-- | verify that an answer is not too long
checkLength :: String -> Maybe String
checkLength s = let l = length s in
  if l >= 20
    then Just "Input data should be no longer than 20 characters (the spaces between the words are counted)!"
    else Nothing

checkAffirmative :: String -> Maybe String
checkAffirmative s =
  if s == "Yes" || s == "yes" || s == "Yes!"
    then Nothing
    else Just "Please try harder, and answer yes."

-- | string constant, name of the html element of the submission indicator
fromSubmit :: String
fromSubmit = "fromSubmit"

-- | string constant, name of the html element of the hidden trace
hiddenTrace :: String
hiddenTrace = "hiddenTrace"

-- | get the id of a question
questionId :: Question -> String
questionId q = "qid-" ++ show (qseq q)

-- | semantics of processing a request
-- 1. 1) get the hidden traces. 2) get the indicator showing whether this is a submission re-
--       quest
-- 2. run the web monad using the trace got from step 1
-- 3. if we get a result, return the result on the final page
-- 4. if we get a question
--    4.1 if we are handling the input from the user: first try to collect and verify the answers provided by the user to the
--        question. If all answers are valid, we add them to the trace, and run the program
--        again (invoking the 'play' function) with the second parameter being set to 'False',
--        to distinguish it from a normal request.
--    4.2 otherwise, we are either running from a inner invocation, or we are dealing with a
--        browser refresh request. In either case, we just return the question to the page.
runWeb :: Web PersonRecord -> ActionM ()
runWeb web = do
  t <- getTraces
  play t True
  where play t first = do
          b <- getSubmit
          r <- liftIO (run web t)
          case r of
            Right pr -> finishPage pr
            Left (q, t) -> 
              if b && first
                then do
                  ga <- gatherAnswers q
                  let as = Prelude.map fst ga
                      ss = Prelude.map snd ga
                      ss' = catMaybes ss
                  if length ss' == 0
                    then play (addAnswer t as) False
                    else questionPage q as ss' t
                else questionPage q [] [] t
        gatherAnswers = mapM findAnswer 

-- | get traces from hidden element
getTraces :: ActionM (Trace Answers)
getTraces = do
  txt <- findByName hiddenTrace
  let str = decode . T.unpack $ txt
  if isEmptyString str 
    then return emptyTrace
    else do let trace = (read str) :: Trace Answers
            liftIO (print trace)
            return $ trace

-- | get the submission indicator, which is a boolean value
getSubmit :: ActionM Bool
getSubmit = do
  t <- findByName fromSubmit
  let s = T.unpack t
  if isEmptyString s 
    then return False
    else return True

-- | find a value associated with a name
findByName :: String -> ActionM T.Text
findByName name = S.param (T.pack name) `rescue` \_ -> return ""

-- | find a potential answer of a question, do not accept empty string as an answer
findAnswer :: Question -> ActionM (QAnswer, Maybe Scold)
findAnswer q = do
  ta <- findByName $ questionId q
  let taStr = T.unpack ta
      scold = verifyInput (verify q) taStr
  case scold of
    Just a -> return ((qseq q, taStr), Just (qseq q, a))
    Nothing -> return ((qseq q, taStr), Nothing)

-- | check empty string
isEmptyString :: String -> Bool
isEmptyString s = not $ s =~ ("\\S+" :: String)


-- | to the question page
questionPage :: Form -> Answers -> [Scold] -> Trace Answers -> ActionM ()
questionPage q a ss t = S.html . renderHtml $ convertToHtml q a ss t

-- | the html structure of the question page
convertToHtml :: Form -> Answers -> [Scold] -> Trace Answers -> Html
convertToHtml q a ss t = do
  docType
  H.form ! A.method "post" $ inputAsFields
  where
    inputAsFields = do
      mapM_ toInputField q 
      H.input ! A.type_ "submit" ! A.value "submit"
      annotateSubmission
      hiddenTraces
    toInputField q = do
      let afq = Map.fromList a
          sfq = Map.fromList ss
          qid = stringValue . questionId $ q
      H.label ! A.for qid $ toHtml (desc q)
      br
      H.input ! A.name qid ! value (case Map.lookup (qseq q) afq of
                                             Just a -> stringValue a
                                             Nothing -> "")
      br
      case Map.lookup (qseq q) sfq of
        Just scold -> H.span ! A.style "color:red" $ H.toHtml scold >> br
        _          -> return ()
      br >> br
    annotateSubmission = H.input ! A.type_ "hidden" ! A.name (H.stringValue fromSubmit) !
      A.value "true"
    hiddenTraces = H.input ! A.type_ "hidden" ! A.name (H.stringValue hiddenTrace) !
      A.value (H.stringValue . encode $ show t)
      
finishPage :: PersonRecord -> ActionM ()
finishPage pr = S.html . renderHtml $ finalPage
  where finalPage = do
          docType
          H.p $ H.span $ "Well done, you've answered all questions, here is your information:"
          br
          H.table $ toTable pr
        toTable pr = do
          H.caption "Personal Information"
          H.tr (H.td "Name" >> (H.td . H.toHtml . prName $ pr))
          H.tr (H.td "Favourite language" >> (H.td . H.toHtml . favLang $ pr))
          H.tr (H.td "Have a pet with name" >> (H.td . H.toHtml . prPet $ pr))
          H.tr (H.td "Sum of favourite top 2 integers" >> (H.td . H.toHtml . show $ (fstInt pr) + (sndInt pr)))
