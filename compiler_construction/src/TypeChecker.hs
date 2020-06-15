module TypeChecker
  (
    module TypeCheckUtil
  , runTypeCheck
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Debug.Trace

import           AbsJavalette
import qualified AbsJavalette as AbsJ
import           Exception
import           PrintJavalette
import           TypeCheckUtil

-- | state monad for type checking, using Env as state
type TypeCheckMonad a = StateT Env IO a

-- | the initial environment
initEnv :: Env
initEnv = Env initFunMap [] Nothing Empty initStructMap Set.empty Set.empty Map.empty

initFunMap :: Map String Type
initFunMap = Map.fromList [ ("printInt", TFun TVoid [TInt])
               , ("printDouble", TFun TVoid [TDoub])
               , ("printString", TFun TVoid [])
               , ("readInt", TFun TInt [])
               , ("readDouble", TFun TDoub [])]

initStructMap :: Map String StructInfo
initStructMap = let
    am = Map.insert "length" (TInt, 0) Map.empty
    ast = StructInfo namingArrayAsStruct am
  in Map.insert namingArrayAsStruct ast Map.empty
  
runTypeCheck :: Prog -> IO Env
runTypeCheck p = execStateT (typeCheck p) initEnv

-- | typeCheck method
typeCheck :: Prog -> TypeCheckMonad ()
typeCheck (Program topDefs) = do
  let functions = [fun | TopDefFun fun <- topDefs]
      structs = [struct | TopDefStruct struct <- topDefs]
      pointers = [pointer | TopDefPointer pointer <- topDefs]
  -- ^ check invalid array type
  liftIO (mapM_ checkTopDefArrayType topDefs)
  -- ^ check and collect program struct/pointer information
  mapM_ checkStructNames structs
  mapM_ checkPointerDefinitions pointers
  mapM_ checkStructAttributes structs
  -- ^ check and collect program function inforamtion
  mapM_ checkFunctionSignature functions
  -- ^ check specification on main function
  checkMain
  -- ^ check function statements
  mapM_ checkFunctionStatements functions

checkStructNames :: Struct -> TypeCheckMonad ()
checkStructNames (StructDef (Ident id) _) = do
  bool <- gets (checkStructExist id)
  case bool of
    True -> throwTce $ "Duplicate struct declaration!\nStruct name: " ++ id
    _ -> do
      sMap <- gets structMap
      let si = StructInfo id Map.empty
          sMap' = Map.insert id si sMap
      modify $ \env -> env {structMap = sMap'}

checkPointerDefinitions :: Pointer -> TypeCheckMonad ()
checkPointerDefinitions (PointerDef (Ident structId) (Ident pointerId)) = do
    bool <- gets (checkStructExist structId)
    case bool of
      False -> throwTce $ "Invalid pointer definition, sturct unknown!\nStruct: " ++ structId ++ ", pointer: " ++ pointerId
      _ -> do
        bool' <- gets (checkPointerExist pointerId)
        case bool' of
          True -> throwTce $ "Duplicate pointer declaration!\nPointer name: " ++ pointerId
          _ -> do
            pMap <- gets pointerMap
            let pMap' = Map.insert pointerId structId pMap
            modify $ \env -> env {pointerMap = pMap'}

checkStructAttributes :: Struct -> TypeCheckMonad ()
checkStructAttributes (StructDef (Ident structId) attributes) = do
    let possibleDuplicateAttr = checkDuplicateAttribute attributes
    case possibleDuplicateAttr of
      Just dupId -> throwTce $ "Duplicate attribute declaration!\nDuplicate attribute name: " ++ dupId ++ ", in struct: " ++ structId
      _ -> do
        let sequencedAttr = zip [0, 1..] attributes
        ls <- mapM collectAttributeInfo sequencedAttr
        let attrMap = Map.fromList ls
            si = StructInfo structId attrMap
        modify (registerStruct si)

collectAttributeInfo :: (Int, Attribute) -> TypeCheckMonad (String, (Type, Int))
collectAttributeInfo (seq, attr) = case attr of
  Attr t (Ident attributeId) -> case t of
    TVoid -> throwTce $ "Invalid attribute type, attribute of a struct can not be type 'void'!\nIn attribute: " ++ printTree attr
    TArray _ -> do
      modify (registerArrayType t)
      return (attributeId, (t, seq))
    _ -> return (attributeId, (t, seq))
  PAttr (Ident pointerId) (Ident attributeId) -> do
    pMap <- gets pointerMap
    case Map.lookup pointerId pMap of
      Nothing -> throwTce $ "Invalid attribute type, no pointer of this type is declared!\nWrong pointerId: " ++ pointerId ++ ", in attribute: " ++ printTree attr
      Just structId -> return (attributeId, (TPointer (Ident structId), seq))

checkFunctionSignature :: Fun -> TypeCheckMonad ()
checkFunctionSignature fun = case fun of
  FnDef t (Ident fid) args _ -> doCheck t fid args
  PFnDef (Ident pointerId) (Ident fid) args _ -> do
    mStructName <- gets (getPointedStructName pointerId)
    case mStructName of
      Nothing -> throwTce $ "Invalid function return pointer type, pointer not defined!\nInvalid pointerId: " ++ pointerId ++ ", in function: " ++ fid
      Just structName -> doCheck (TPointer (Ident structName)) fid args
  where
    doCheck :: Type -> String -> [Arg] -> TypeCheckMonad ()
    doCheck t fid args = do
      funMap <- gets funs
      case Map.member fid funMap of
        True -> throwTce $ "Duplicate function definition!\nFunction name: " ++ fid
        _ -> do
          argumentTypes <- mapM extractArgumentType args
          let result = triggerChecking argumentTypes
          case result of
            True -> do
              let functionType = TFun t argumentTypes
                  newFuncMap = Map.insert fid functionType funMap
              modify (\env -> env {funs = newFuncMap})

extractArgumentType :: Arg -> TypeCheckMonad Type
extractArgumentType arg = case arg of
  Argument t (Ident id) -> case t of
    TVoid -> throwTce $ "Illegal type, variable type can not be 'void'!\nVariable: " ++ id
    _ -> return t
  PArgument (Ident pointerId) (Ident id) -> do
    mStructName <- gets (getPointedStructName pointerId)
    case mStructName of
      Nothing -> throwTce $ "Invalid pointer type, pointer with name: " ++ pointerId ++ " does not exist!\nIn argument: " ++ printTree arg
      Just structName -> return (TPointer (Ident structName))
    
triggerChecking :: [Type] -> Bool
triggerChecking ts = and [if i == TVoid then False else True | i <- ts]
          
checkMain :: TypeCheckMonad ()
checkMain = do
  env <- get
  let funMap = funs env
      mfm = Map.lookup "main" funMap
  case mfm of
    Nothing -> throwError $ "'main' function is misssing!"
    Just (TFun t ts) -> case t of
      TInt -> case ts of
        [] -> return ()
        _  -> throwError "'main' function can not have any input parameters!"
      _   -> throwError "'main' function can only return type 'int'!"

checkFunctionStatements :: Fun -> TypeCheckMonad ()
checkFunctionStatements fun = case fun of
  FnDef t (Ident fid) as (Block bs) -> doCheck t fid as bs
  PFnDef (Ident pointerId) (Ident fid) as (Block bs) -> do
    mStructName <- gets (getPointedStructName pointerId)
    case mStructName of
      Just structName -> let t = TPointer (Ident structName) in doCheck t fid as bs
  where
    doCheck :: Type -> String -> [Arg] -> [Stmt] -> TypeCheckMonad ()
    doCheck t fid args stms = do
      modify enterBlock
      modify (setCurrentFunction fun)
      mapM_ checkArgumentVariable args
      mapM_ checkStatement stms
      checkReturnStatus t fid  (Block stms)
      modify unsetCurrentFunction
      modify exitBlock

checkReturnStatus :: Type -> String -> Blk -> TypeCheckMonad ()
checkReturnStatus t fid blk = case t of
  TVoid -> return ()
  _ -> do
    let b = checkAllPathsAreReturned t blk
    case b of
      False -> throwTce $ "Some execution paths are not covered by a return statement!\nIn function: " ++ fid
      _ -> return ()

checkAllPathsAreReturned :: Type -> Blk -> Bool
checkAllPathsAreReturned t (Block stms) = foldr checkStmReturned False stms
  where
    checkStmReturned :: Stmt -> Bool -> Bool
    checkStmReturned stm currentState = case currentState of
      True -> True
      _ -> case stm of
        BStmt blk  -> checkAllPathsAreReturned t blk
        While expr stm -> case expr of
          ELitTrue -> checkAllPathsAreReturned t (Block [stm])
          _        -> False
        CondElse expr stm1 stm2 -> case expr of
          ELitTrue -> checkAllPathsAreReturned t (Block [stm1])
          ELitFalse -> checkAllPathsAreReturned t (Block [stm2])
          _        -> and (fmap (checkAllPathsAreReturned t) [Block [stm1], Block [stm2]])
        Cond expr stm -> case expr of
          ELitTrue -> checkAllPathsAreReturned t (Block [stm])
          _        -> False
        ForEach _ _ _ stm' -> checkAllPathsAreReturned t (Block [stm'])
        ReturnE _  -> True
        _          -> False
                
checkArgumentVariable :: Arg -> TypeCheckMonad ()
checkArgumentVariable arg = case arg of
  Argument t (Ident id) -> doCheck t id
  PArgument (Ident pointerId) (Ident id) -> do
    mStructName <- gets (getPointedStructName pointerId)
    case mStructName of
      Just structName -> let t = TPointer (Ident structName) in doCheck t id
  where
    doCheck :: Type -> String -> TypeCheckMonad ()
    doCheck t id = checkThenAddVariable id t True

checkStatementBlock :: Stmt -> TypeCheckMonad ()
checkStatementBlock stmt = do
  modify enterBlock
  checkStatement stmt
  modify exitBlock

checkStatementListBlock :: [Stmt] -> TypeCheckMonad ()
checkStatementListBlock stms = do
  modify enterBlock
  mapM_ checkStatement stms
  modify exitBlock
    
checkStatement :: Stmt -> TypeCheckMonad ()
checkStatement stm = do
  modify (\env -> env {currentStatement = stm})
  returnType <- gets getCurrentFunctionReturnType
  case stm of
    Empty -> return ()
    BStmt (Block ss) -> checkStatementListBlock ss
    Decl t items -> do
      mapM_ (checkDeclarationItems t) items
    PDecl (Ident pointerId) items -> do
      mStructName <- gets (getPointedStructName pointerId)
      case mStructName of
        Just structName -> mapM_ (checkDeclarationItems (TPointer (Ident structName))) items
        _ -> throwError $ "Pointer not defined: " ++ pointerId
    Ass (Ident id) expr -> do
      mt <- gets (findVarType id)
      case mt of
        Nothing -> throwError $ "Usage of undeclared variable!\nVariable: " ++ id
        Just t -> do
          t' <- checkExpression expr
          if t' == t
            then case t of
              TArray _ -> modify (markAsInitialized id)
              TPointer _ -> modify (markAsInitialized id)
              _ -> return ()              
            else throwError $ "Type mismatch, expected: " ++ showType t ++ ", actual: " ++ showType t'
    AssElem expr1 expr2 -> case expr1 of
      EIndex _ _ -> do
        t1 <- checkExpression expr1
        t2 <- checkExpression expr2
        if t1 /= t2
          then throwError $ "Type mismatch, expected: " ++ showType t1 ++ ", actual: " ++ showType t2
          else return ()
      EDeref expr (Ident attr) -> do
        t1 <- checkExpression expr1
        t2 <- checkExpression expr2
        if t1 /= t2
          then throwError $ "Type mismatch, expected: " ++ showType t1 ++ ", actual: " ++ showType t2
          else return ()
      _ -> throwError "Invalid expression assignment!"
    Incr (Ident id) -> do
      mt <- gets (findVarType id)
      case mt of
        Nothing -> throwError $ "Usage of undeclared variable!\nVariable: " ++ id
        Just t -> case t of
          TInt -> return ()
          _    -> throwError "Type mismatch, '++' can only be applied on variable whose type is int!" 
    Decr (Ident id) -> do
      mt <- gets (findVarType id)
      case mt of
        Nothing -> throwError $ "Usage of undeclared variable!\nVariable: " ++ id 
        Just t -> case t of
          TInt -> return ()
          _    -> throwError "Type mismatch, '++' can only be applied on variable whose type is int!"
    ReturnE expr -> do
      case returnType of
        TVoid -> throwError "Return type mismatch!"
        _ -> do
          t' <- checkExpression expr
          if returnType == t'
            then return ()
            else throwError $ "Function return type mismatch, expected: " ++ showType returnType ++ " , actual: " ++ showType t'  
    ReturnV -> case returnType of
      TVoid -> return ()
      _     -> throwError $ "Function return type mismatch, return value is expected!"  
    Cond expr stm' -> do
      exprType <- checkExpression expr
      case exprType of
        TBool -> checkStatementBlock stm'
        _     -> throwError $ "Type mismatch, expression of 'If' statement must be of type boolean!" 
    CondElse expr stm1 stm2 -> do
      exprType <- checkExpression expr
      case exprType of
        TBool -> mapM_ checkStatementBlock [stm1, stm2]
        _ -> throwError $ "Type mismatch, expression of 'If else' statement must be of type boolean!" 
    While expr stm' -> do
      exprType <- checkExpression expr
      case exprType of
        TBool -> checkStatementBlock stm'
        _ -> throwError $ "Type mismatch, expression of 'While' statement must be of type boolean!"  
    ForEach t (Ident id) expr stm -> do
      exprType <- checkExpression expr
      case exprType of
        TArray t' ->
          if t == t'
            then do modify enterBlock
                    checkThenAddVariable id t True
                    case stm of
                      BStmt (Block stms) -> mapM_ checkStatement stms
                      _ -> checkStatement stm
                    modify exitBlock
            else throwError $ "Type mismatch inside for-each loop!" 
        _ -> throwError $ "Expression must be evaluated as an array inside for-each loop!"
    Sexp expr -> case expr of
      EAPP _ _ -> do
        exprt <- checkExpression expr
        case exprt of
          TVoid -> return ()
          _ -> throwError "Type mismatch, expression as statement only limits to 'void' function invocation!"  
      _ -> throwError "Type mismatch, expression as statement only limits to 'void' function invocation!" 

-- | type check expression
checkExpression :: Expr -> TypeCheckMonad Type
checkExpression exp = case exp of
  Evar (Ident id) -> do
    mt <- gets (findVarType id)
    case mt of
      Nothing -> throwError $ "Usage of undeclared variable!\nVariable: " ++ id 
      Just t -> do
        uninitBool <- gets (isUninitialized id) 
        if uninitBool == True
          then throwError $ "Uninitialized usage of variable!\nVariable: " ++ id
          else return t
  ENew object -> case object of
    ArrayObj array -> checkArrayType array
    StructObj (Ident structName) -> do
      bool <- gets (checkStructExist structName)
      case bool of
        True -> return (TPointer (Ident structName))
        _ -> throwError $ "Unknown struct name: " ++ structName
  ELitInt _  -> return TInt
  ELitDoub _ -> return TDoub
  ELitTrue   -> return TBool
  ELitFalse  -> return TBool
  EAPP (Ident fid) exprs -> do
    mft <- gets (getFunctionType fid)
    case mft of
      Nothing -> throwError "Undeclared function invocation!" 
      Just (TFun rt ats) -> case fid of
        "printString" -> case exprs of
           [EString ps] -> return TVoid
           _ -> throwError "Function 'printString' only accepts one string parameter!" 
        _ -> do
          b <- checkArgumentsTypeMatch fid ats exprs
          case b of
            True -> return rt
  EString str -> throwError "String literal should only be used as input parameter of function 'printString'!"
  ENull (Ident pointerId) -> do
    mStructName <- gets (getPointedStructName pointerId)
    case mStructName of
      Just structName -> return (TPointer (Ident structName))
      _ -> throwError $ "Unknown pointer!\nPointer name: " ++ pointerId
  EDeref expr (Ident attrId) -> do
    t <- checkExpression expr
    attributeTypeCheck t attrId
  EDot expr (Ident attrId) -> do
    t <- checkExpression expr
    attributeTypeCheck t attrId 
  EIndex expr1 expr2 -> do
    t1 <- checkExpression expr1
    case t1 of
      TArray t -> do
        t2 <- checkExpression expr2
        case t2 of
          TInt -> return t
          _ -> throwError "Invalid index type, index must be an expression evaluated to be of type int!" 
      _ -> throwError "Feature not implemented yet!" 
  Neg expr -> do
    t <- checkExpression expr
    if t /= TInt && t /= TDoub
      then throwError "Operator '-' can only be used on int or double!" 
      else return t
  Not expr -> do
    t <- checkExpression expr
    if t /= TBool
      then throwError "Operator '!' can only be used on boolean!"
      else return TBool
  EMul expr1 op expr2
    | (op == Times || op == Div) -> matchOptrType "'*', '/'" (Set.fromList [TInt, TDoub]) Nothing expr1 expr2
    | otherwise -> matchOptrType "'%'" (Set.fromList [TInt]) Nothing expr1 expr2
  EAdd expr1 op expr2 -> matchOptrType "'+', '-'" (Set.fromList [TInt, TDoub]) Nothing expr1 expr2
  ERel expr1 op expr2
    | Set.member op (Set.fromList [AbsJ.Lt, AbsJ.Le, AbsJ.Gt, AbsJ.Ge]) -> do
        matchOptrType "'<', '<=', '>', '>='" (Set.fromList [TInt, TDoub]) Nothing expr1 expr2
        return TBool
    | otherwise -> do
        matchOptrType "'==', '!='" (Set.fromList [TInt, TDoub, TBool]) (Just (TPointer (Ident ""))) expr1 expr2
        return TBool
  EAnd expr1 expr2 -> matchOptrType "'&&'" (Set.fromList [TBool]) Nothing expr1 expr2
  EOr expr1 expr2  -> matchOptrType "'||'" (Set.fromList [TBool]) Nothing expr1 expr2

-- | check variable declaration
checkDeclarationItems :: Type -> Item -> TypeCheckMonad ()
checkDeclarationItems t item = case item of
  NonInit (Ident id)  -> checkThenAddVariable id t False
  Init (Ident id) expr -> do
    exprType <- checkExpression expr
    if t == exprType
      then checkThenAddVariable id t True
      else throwError $ "Type mismatch on variable: " ++ id ++ ". Expected: " ++ showType t ++ ", actual: " ++ showType exprType   

-- | check variable type and scope
checkThenAddVariable :: String -> Type -> Bool -> TypeCheckMonad ()
checkThenAddVariable id t alreadyInitialized = case t of
  TVoid -> throwError $ "Variable type can not be 'void'!\nVariable: " ++ id 
  _ -> do
    b <- gets (existInTopBlock id)
    case b of
      True -> throwError $ "Variable declaration duplication!\nVariable:  " ++ id 
      _ -> do
        modify (addVarType id t)
        case t of
          TArray _ -> do
            modify (registerArrayType t)
            if alreadyInitialized == True
              then return ()
              else modify (addToUninitSet id)
          TPointer _ -> do
            if alreadyInitialized == True
              then return ()
              else modify (addToUninitSet id)
          _ -> return ()

-- | type check array type
checkArrayType :: Array -> TypeCheckMonad Type
checkArrayType array = do
  case array of
    SArray t expr -> do
      indexType <- checkExpression expr
      case indexType of
        TInt -> return (TArray t)
        _ -> throwError $ "Indexing expression must be of type 'int'!" 
    MArray innerArray expr -> do
      innerType <- checkArrayType innerArray
      indexType <- checkExpression expr
      case indexType of
        TInt -> return (TArray innerType)
        _ -> throwError $ "Indexing expression must be of type 'int'!" 
            
checkArgumentsTypeMatch :: String -> [Type] -> [Expr] -> TypeCheckMonad Bool
checkArgumentsTypeMatch fid ats exprs = do
  let l1 = length ats
      l2 = length exprs
  if l1 /= l2
    then throwError $ "Parameter size mismatch, expected: " ++ show l1 ++ " , actual: " ++ show l2
    else do
      let xs = zip [1..] $ zip ats exprs
      rs <- mapM (matching fid) xs
      return $ and rs
  where
    matching :: String -> (Int, (Type, Expr)) -> TypeCheckMonad Bool
    matching fid (i, (t, expr)) = do
      exprType <- checkExpression expr
      if t == exprType
        then return True
        else throwError $ "Type mismatch, expected: " ++ showType t ++ ", actual: " ++ showType exprType ++ ", at the " ++ show i ++ "th parameter of function  " ++ fid      
                
matchOptrType :: String -> Set Type -> Maybe Type -> Expr -> Expr -> TypeCheckMonad Type
matchOptrType optStr availableTypes mt expr1 expr2 = do
  t1 <- checkExpression expr1
  t2 <- checkExpression expr2
  if t1 /= t2
    then throwError $ "Type mismatch, expr1 has type: " ++ showType t1 ++ " , expr2 has type: " ++ showType t2  
    else if Set.notMember t1 availableTypes && not (sameGroupOfType mt t1)  
            then throwError $ optStr ++ " can only be applied on type " ++ printTypeSet availableTypes  
            else return t1
  where               
    printTypeSet :: Set Type -> String
    printTypeSet typeSet = let result = Set.foldr convertTypeToString "" typeSet in init result

    convertTypeToString :: Type -> String -> String
    convertTypeToString t s = case t of
      TInt     -> " int," ++ s
      TDoub    -> " double," ++ s
      TBool    -> " boolean," ++ s
      TVoid    -> " void," ++ s
      _ -> s
              
    sameGroupOfType :: Maybe Type -> Type -> Bool
    sameGroupOfType mt t = case mt of
      Nothing -> False
      Just t' -> case t' of
        TPointer _ -> case t of
          TPointer _ -> True
          _ -> False
        _ -> False

attributeTypeCheck :: Type -> String -> TypeCheckMonad Type
attributeTypeCheck t attrId = case t of
  TArray _ -> do
    let structName = namingArrayAsStruct
    mt <- gets (getAttributeInfo structName attrId)
    case mt of
      Nothing -> throwError $ "Invalid attribute access, attribute with name '" ++ attrId ++ "' doesn't exist in struct: " ++ structName
      Just (t', _) -> return t'
  TPointer (Ident s) -> do
    let structName = s
    mt <- gets (getAttributeInfo structName attrId)
    case mt of
      Nothing -> throwError $ "Invalid attribute access, attribute with name '" ++ attrId ++ "' doesn't exist in struct: " ++ structName
      Just (t', _) -> return t'
  _ -> throwError $ "Invalid attribute access! No struct or array type found!"
    
throwError :: String -> TypeCheckMonad a
throwError s = do
  currentFun <- gets getCurrentFunctionName
  currentStm <- gets currentStatement
  throwTce $ s ++ "\nIn statement:\n\t" ++ printTree currentStm ++ "In function: " ++ currentFun
