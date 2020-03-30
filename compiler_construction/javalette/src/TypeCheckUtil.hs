module TypeCheckUtil where

import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           AbsJavalette

-- | environment used for type checking. funs - map for functions, vars - map for variables
data Env = Env {
    funs :: Map String Type
  , vars :: [Map String Type]
  , currentF :: Maybe Fun} deriving Show

enterBlock :: Env -> Env
enterBlock env = let stack = Map.empty : (vars env) in env {vars = stack}

exitBlock :: Env -> Env
exitBlock env = let stack = drop 1 (vars env) in env {vars = stack}

setCurrentF :: Fun -> Env -> Env
setCurrentF fun env = env {currentF = Just fun}

unsetCurrentF :: Env -> Env
unsetCurrentF env = env {currentF = Nothing}

getFunctionName :: Fun -> String
getFunctionName (FnDef _ (Ident id) _ _) = id

getCurrentFunName :: Env -> String
getCurrentFunName env = let curf = currentF env in
  case curf of
    Nothing    -> ""
    Just fun -> getFunctionName fun

existInTopBlock :: String -> Env -> Bool
existInTopBlock id env = let stack = vars env in
  case stack of
    [] -> False
    (x : xs) -> let v = Map.lookup id x in
      case v of
        Nothing -> False
        _       -> True

findFunType :: String -> Env -> Maybe Type
findFunType fid env = let funMap = funs env in Map.lookup fid funMap

findVarType :: String -> Env -> Maybe Type
findVarType id env = let stack = vars env in
  case stack of
    [] -> Nothing
    (m : ms) -> let v = Map.lookup id m in
      case v of
        Just _ -> v
        _      -> findVarType id (env {vars = ms})

addVarType :: String -> Type -> Env -> Env
addVarType id t env =
  let stack = vars env
      top = head stack
      top' = Map.insert id t top
      stack' = top' : (drop 1 stack)
  in env {vars = stack'}

emptyEnv :: Env
emptyEnv = Env Map.empty [] Nothing

showType :: Type -> String
showType t = case t of
  TInt  -> "int"
  TDoub -> "double"
  TBool -> "boolean"
  TVoid -> "void"
  TFun _ _ -> ""
