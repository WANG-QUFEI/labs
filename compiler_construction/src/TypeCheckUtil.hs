module TypeCheckUtil where
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           AbsJavalette
import           Exception
import           PrintJavalette

-- | environment used for type checking. funs - map for functions, vars - map for variables
data Env = Env {
  -- | program function inforamtion
  funs :: Map String Type,
  -- | function variable information
  vars :: [Map String Type],
  -- | current function scope in which the type checking works
  currentFunction :: Maybe Fun,
  -- | current statement scope in which the type checking works
  currentStatement :: Stmt,
  -- | struct information
  structMap :: Map String StructInfo,
  -- | a set of type TArray used for later code generation
  arrayTypes :: Set Type,
  -- | a set recording uninitialized complex type
  uninitSet :: Set String,
  -- | program pointer information
  pointerMap :: Map String String
} deriving Show

-- | data structure storing struct information
data StructInfo = StructInfo {
  name    :: String,
  attrMap :: Map String (Type, Int)
} deriving Show

namingArrayAsStruct :: String
namingArrayAsStruct = "0_0:array"

registerStruct :: StructInfo -> Env -> Env
registerStruct struct env =
  let strtMap  = structMap env
      strtMap' = Map.insert (name struct) struct strtMap
  in env {structMap = strtMap'}

registerArrayType :: Type -> Env -> Env
registerArrayType t env = case t of
  TArray _ -> let arrayTypeSet = arrayTypes env
                  newSet = Set.insert t arrayTypeSet
              in env {arrayTypes = newSet}

inquireStructName :: Type -> Maybe String
inquireStructName t = case t of
  TPointer (Ident name) -> Just name
  TArray _     -> Just namingArrayAsStruct
  _            -> Nothing
  
addToUninitSet :: String -> Env -> Env
addToUninitSet id env =
  let set = uninitSet env
      set' = Set.insert id set
  in env {uninitSet = set'}

markAsInitialized :: String -> Env -> Env
markAsInitialized id env =
  let set = uninitSet env
      set' = Set.delete id set
  in env {uninitSet = set'}

isUninitialized :: String -> Env -> Bool
isUninitialized id env = let set = uninitSet env in Set.member id set
  
-- | get struct attribute type
getAttributeInfo :: String -> String -> Env -> Maybe (Type, Int)
getAttributeInfo structName attrName env =
  let sMap = structMap env
  in case Map.lookup structName sMap of
    Nothing -> Nothing
    Just struct -> let aMap = attrMap struct in case Map.lookup attrName aMap of
      Nothing -> Nothing
      x -> x

enterBlock :: Env -> Env
enterBlock env = let stack = Map.empty : (vars env) in env {vars = stack}

exitBlock :: Env -> Env
exitBlock env = let stack = drop 1 (vars env) in env {vars = stack}

setCurrentFunction :: Fun -> Env -> Env
setCurrentFunction fun env = env {currentFunction = Just fun}

unsetCurrentFunction :: Env -> Env
unsetCurrentFunction env = env {currentFunction = Nothing}

getCurrentFunctionName :: Env -> String
getCurrentFunctionName env = let curfun = currentFunction env in
  case curfun of
    Nothing    -> ""
    Just f -> getFunctionName f

getCurrentFunctionReturnType :: Env -> Type
getCurrentFunctionReturnType env =
  let Just fun = currentFunction env in getFunctionReturnType fun env

getFunctionName :: Fun -> String
getFunctionName fun = case fun of
  FnDef _ (Ident id) _ _ -> id
  PFnDef _ (Ident id) _ _ -> id

getFunctionReturnType :: Fun -> Env -> Type
getFunctionReturnType fun env = case fun of
  FnDef t _ _ _ -> t
  PFnDef _ (Ident fid) _ _ ->
    let funMap = funs env
        Just ft = Map.lookup fid funMap
    in case ft of
      TFun t _ -> t

existInTopBlock :: String -> Env -> Bool
existInTopBlock id env = let stack = vars env in
  case stack of
    [] -> False
    (x : xs) -> let v = Map.lookup id x in
      case v of
        Nothing -> False
        _       -> True

getFunctionType :: String -> Env -> Maybe Type
getFunctionType fid env = let funMap = funs env in Map.lookup fid funMap

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

showType :: Type -> String
showType t = case t of
  TInt  -> "int"
  TDoub -> "double"
  TBool -> "boolean"
  TVoid -> "void"
  TArray t' -> showType t' ++ "[]"
  TFun _ _ -> ""
  TPointer (Ident s) -> "struct*" ++ s

checkTopDefArrayType :: TopDef -> IO ()
checkTopDefArrayType topDef = case topDef of
  TopDefFun fun -> checkFunctionArrayType fun
  TopDefStruct struct -> checkStructArrayType struct
  TopDefPointer _ -> return ()
    
checkFunctionArrayType :: Fun -> IO ()
checkFunctionArrayType fun = case fun of
  PFnDef _ (Ident fid) args (Block stms) -> do
    mapM_ (checkArgArrayType fid) args
    mapM_ (checkStmArrayType fid) stms                
  FnDef t (Ident fid) args (Block stms) -> case checkValidityIfTypeIsArray t of
    False -> throwTce $ "Invalid array type found in the return type of function: " ++ fid
    _ -> do
      mapM_ (checkArgArrayType fid) args
      mapM_ (checkStmArrayType fid) stms
    
checkStructArrayType :: Struct -> IO ()
checkStructArrayType (StructDef (Ident id) attrs) = mapM_ (checkAttributeArrayType id) attrs

checkValidityIfTypeIsArray :: Type -> Bool
checkValidityIfTypeIsArray t = case t of
    TArray t' -> notVoidType t'
    _ -> True
  where notVoidType :: Type -> Bool
        notVoidType t = case t of
          TVoid -> False
          TArray t' -> notVoidType t'
          _ -> True

checkArrayObjectType :: Array -> Bool
checkArrayObjectType array = case array of
  SArray t _ -> case t of
    TVoid -> False
    TArray _ -> False
    TFun _ _ -> False
    _ -> True
  MArray innerArray _ -> checkArrayObjectType innerArray

checkAttributeArrayType :: String -> Attribute -> IO ()
checkAttributeArrayType structId attribute = case attribute of
  Attr t _ -> case checkValidityIfTypeIsArray t of
    False -> throwTce $ "Invalid array type found in attributes of struct: " ++ structId ++ "!\nIn attribute: " ++ printTree attribute
    _ -> return ()
  _ -> return ()
      
checkArgArrayType :: String -> Arg -> IO ()
checkArgArrayType fid arg = case arg of
  PArgument _ _ -> return ()
  Argument t (Ident argId) -> case checkValidityIfTypeIsArray t of
    False -> throwTce $ "Invalid array type found in the argument with id: " ++ argId ++ " from function: " ++ fid ++ "\n\tIn argument: " ++ printTree (Argument t (Ident argId))
    _ -> return ()

checkStmArrayType :: String -> Stmt -> IO ()
checkStmArrayType fid stm = case stm of
  BStmt (Block stms) -> mapM_ (checkStmArrayType fid) stms
  Decl t items -> case checkValidityIfTypeIsArray t of
    False -> throwTce $ "Invalid array type found in statements of function " ++ fid ++ ":\nIn statement: " ++ printTree stm
    _ -> mapM_ (checkItemArrayType fid) items
  PDecl _ items -> mapM_ (checkItemArrayType fid) items
  Ass _ expr -> checkExprArrayType fid expr
  AssElem expr1 expr2 -> mapM_ (checkExprArrayType fid) [expr1, expr2]
  ReturnE expr -> checkExprArrayType fid expr
  Cond expr stm -> do
    checkExprArrayType fid expr
    checkStmArrayType fid stm
  CondElse expr stm1 stm2 -> do
    checkExprArrayType fid expr
    mapM_ (checkStmArrayType fid) [stm1, stm2]
  While expr stm -> do
    checkExprArrayType fid expr
    checkStmArrayType fid stm
  ForEach t _ expr stm' -> case checkValidityIfTypeIsArray (TArray t) of
    False -> throwTce $ "Invalid array type found in statement of function: " ++ fid ++ ":\nIn statement: " ++ printTree stm
    _ -> do
      checkExprArrayType fid expr
      checkStmArrayType fid stm'
  _ -> return ()
        
checkItemArrayType :: String -> Item -> IO ()
checkItemArrayType fid item = case item of
  NonInit _ -> return ()
  Init _ expr -> checkExprArrayType fid expr
      
checkExprArrayType :: String -> Expr -> IO ()
checkExprArrayType fid expr = case expr of
  ENew object -> case object of
    ArrayObj array -> case checkArrayObjectType array of
      False -> throwTce $ "Invalid array type found in expressions of function " ++ fid ++ "!\nIn expression: " ++ printTree expr
      _ -> return ()
    StructObj _ -> return ()
  EAPP _ exprs -> mapM_ (checkExprArrayType fid) exprs
  EDeref expr _ -> checkExprArrayType fid expr
  EDot expr _ -> checkExprArrayType fid expr
  EIndex expr1 expr2 -> mapM_ (checkExprArrayType fid) [expr1, expr2]
  Neg expr -> checkExprArrayType fid expr
  Not expr -> checkExprArrayType fid expr
  EMul expr1 _ expr2 -> mapM_ (checkExprArrayType fid) [expr1, expr2]
  EAdd expr1 _ expr2 -> mapM_ (checkExprArrayType fid) [expr1, expr2]
  ERel expr1 _ expr2 -> mapM_ (checkExprArrayType fid) [expr1, expr2]
  EAnd expr1 expr2 -> mapM_ (checkExprArrayType fid) [expr1, expr2]
  EOr expr1 expr2 -> mapM_ (checkExprArrayType fid) [expr1, expr2]
  _ -> return ()

checkDuplicateAttribute :: [Attribute] -> Maybe String
checkDuplicateAttribute attrlist =
  let leftOrRight = foldr checkDuplicate (Left Set.empty) attrlist
  in case leftOrRight of
    Right s -> Just s
    _ -> Nothing
  where checkDuplicate :: Attribute -> (Either (Set String) String) -> (Either (Set String) String)
        checkDuplicate attr leftRight = case leftRight of
          Right s -> Right s
          Left set ->
            let attrId = case attr of
                  Attr _ (Ident id) -> id
                  PAttr _ (Ident id) -> id
            in case Set.member attrId set of
              True -> Right attrId
              _ -> Left (Set.insert attrId set)

getPointedStructName :: String -> Env -> Maybe String
getPointedStructName pointerId env =
  let pMap = pointerMap env
  in Map.lookup pointerId pMap

checkStructExist :: String -> Env -> Bool
checkStructExist id env =
  let sMap = structMap env in Map.member id sMap

checkPointerExist :: String -> Env -> Bool
checkPointerExist id env =
  let pMap = pointerMap env in Map.member id pMap
