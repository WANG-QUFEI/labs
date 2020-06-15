{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CodeGen
  (
    runCodeGenerate
  ) where

import           Control.Monad
import           Control.Monad.Writer.Strict
import           Control.Monad.State.Strict
import           Control.Monad.Fail
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe
import           Data.Either
import           Data.Text (Text)
import qualified Data.Text as T

import           AbsJavalette
import qualified AbsJavalette as AbsJ
import           CodeGenUtil
import           Exception
import qualified TypeCheckUtil as TCU          

-- | monad for code generation, including a Writer monad
newtype CodeGenMonad a = MkCodeGen (StateT Env (Writer Text) a)
  deriving (Functor, Applicative, Monad, MonadState Env, MonadWriter Text)

instance MonadFail CodeGenMonad where
  fail s = throwCge s

-- | API interface of this module used to generate llvm IR code
runCodeGenerate :: Prog -> TCU.Env -> IO (Prog, Text)
runCodeGenerate p checkEnv = do
  let r = codeGenerate p checkEnv
      (p', t) = evalCodeGen r
  --  env = debugCodeGen r
  --  putStrLn $ show (varM env)
  return (p', t)

-- | get the value from 'CodeGenMoand'
evalCodeGen :: CodeGenMonad a -> (a, Text)
evalCodeGen (MkCodeGen st) = runWriter (evalStateT st emptyEnv)

debugCodeGen :: CodeGenMonad a -> Env
debugCodeGen (MkCodeGen st) = fst (runWriter (execStateT st emptyEnv))

-- | a monad computation which actually generates the llvm code
codeGenerate :: Prog -> TCU.Env -> CodeGenMonad Prog
codeGenerate (Program topDefs) checkEnv = do
  let functions = [f | TopDefFun f <- topDefs]
      structs = [s | TopDefStruct s <- topDefs]
      pointers = [p | TopDefPointer p <- topDefs]
  modify (informationShare checkEnv)
  structMap <- gets structMap
  generatePrimitiveFuncDecl functions
  generateStringLiteralDecl functions
  generateStructureTypes (Map.elems structMap)
  fs <- generateFunctions functions
  let topfs = fmap (\x -> TopDefFun x) fs
      topSs = fmap (\x -> TopDefStruct x) structs
      topps = fmap (\x -> TopDefPointer x) pointers
  return (Program (topSs ++ topps ++ topfs))

-- | generate needed primitive function declarations
generatePrimitiveFuncDecl :: [Fun] -> CodeGenMonad ()
generatePrimitiveFuncDecl fs = do
  tell . T.unlines $ fmap llvmFuncForm $ progPrimitiveFunc fs
  tell (T.pack "\n")

-- | generate string declaration
generateStringLiteralDecl :: [Fun] -> CodeGenMonad ()
generateStringLiteralDecl fs = do
  let tup    = fmap llvmStringForm $ zip [0, 1..] (progString fs)
      txts   = fmap fst tup
      strMap = Map.fromList $ fmap snd tup
  tell . T.unlines $ txts
  modify (saveStringLiteral strMap)

-- | generate llvm structure types (including arrays and multi-mensional arrays)
generateStructureTypes :: [TCU.StructInfo] -> CodeGenMonad ()
generateStructureTypes structs = generateArrays >> generateStructures structs
  where
    generateArrays :: CodeGenMonad ()
    generateArrays = do
      env <- get
      let Left arrayTypeSet = arrayTypeInfo env
          (txt, arrayTypeMap) = llvmArrayTypes arrayTypeSet
      modify $ \env -> env {arrayTypeInfo = Right arrayTypeMap}
      tell txt

    generateStructures :: [TCU.StructInfo] -> CodeGenMonad ()
    generateStructures structs = do
      Right aMap <- gets arrayTypeInfo 
      let txt = llvmStructs aMap structs
      tell txt

-- | generate function definitions
generateFunctions :: [Fun] -> CodeGenMonad [Fun]
generateFunctions fs = do
  fs' <- mapM generatefun fs
  return fs'
  
-- | generate a single function definition
generatefun :: Fun -> CodeGenMonad Fun
generatefun f = do
  functionMap <- gets funM
  sMap <- gets structMap
  pMap <- gets pointerMap
  let f0 = reverseArrayOrder . getRenamedFunction . filterUnreachableStm . filterRedundantBoolLit . filterEmptyStm $ f
      f1 = getAnnotatedFunction functionMap sMap pMap f0
  modify envRefresh
  headTxt <- generateHead f1
  generateVariableAllocation f1
  generateArgsInit f1
  generateFunctionStms f1
  generateRetInstruction
  txtlist <- gets (Map.elems . blkM)
  tell $ headTxt <> (T.concat txtlist)
  return f0
  
-- | generate head instruction of a function
generateHead :: Fun -> CodeGenMonad Text
generateHead f = do
  Right tMap <- gets arrayTypeInfo
  fMap <- gets funM
  let (headTxt, retTypeStr) = llvmFunctionHead fMap tMap f
  modify (\x -> x {retType = retTypeStr})
  return $ T.pack "\n" <> headTxt 

-- | generate all variable allocation instruction
generateVariableAllocation :: Fun -> CodeGenMonad ()
generateVariableAllocation fun = do
  funMap <- gets funM
  case fun of
    FnDef t (Ident id) args (Block stms) -> do
      let Just ft = Map.lookup id funMap
      doGenerate ft args stms
    PFnDef pid (Ident fid) args (Block stms) -> do
      let Just ft = Map.lookup fid funMap
      doGenerate ft args stms
  where doGenerate :: Type -> [Arg] -> [Stmt] -> CodeGenMonad ()
        doGenerate funType args stms = case funType of
          TFun t argts -> do
            argIdTypeList <- getArgIdTypeList args argts
            localIdTypeList <- getLocalVarIdTypeList stms
            modify (setEnvNext (length args))
            modify newLabel
            (env, reservedVarTxt) <- gets (generateReturnVal t)
            put env
            allocaVarTxts <- mapM allocaVar (argIdTypeList ++ localIdTypeList)
            modify . saveText $ reservedVarTxt <> (T.concat allocaVarTxts)

getArgIdTypeList :: [Arg] -> [Type] -> CodeGenMonad [(String, String)]
getArgIdTypeList args argts = do
  Right tMap <- gets arrayTypeInfo
  let tup0 = zip args argts
      tup1 = fmap (\e -> case e of
                        (Argument _ (Ident argId), argType) -> (argId, argType)
                        (PArgument _ (Ident argId), argType) -> (argId, argType)) tup0
      tup2 = fmap (\(id, t) -> (id, typeTransform t tMap)) tup1
  return tup2

getLocalVarIdTypeList :: [Stmt] -> CodeGenMonad [(String, String)]
getLocalVarIdTypeList stms = do
  doubList <- mapM getStmVarIdTypeList stms
  return $ concat doubList

getStmVarIdTypeList :: Stmt -> CodeGenMonad [(String, String)]
getStmVarIdTypeList stm = do
  Right tMap <- gets arrayTypeInfo
  pMap <- gets pointerMap
  case stm of
    Decl t items ->
      let idlist = fmap getItemId items
          tString = typeTransform t tMap
          idTypeList = fmap (\x -> (x, tString)) idlist
      in return idTypeList
    PDecl (Ident pid) items ->
      let idlist = fmap getItemId items
          Just structName = Map.lookup pid pMap
          tString = typeTransform (TPointer (Ident structName)) tMap
          idTypeList = fmap (\x -> (x, tString)) idlist
      in return idTypeList
    BStmt (Block stms) -> do
      doubList <- mapM getStmVarIdTypeList stms
      return $ concat doubList
    Cond _ stm' -> getStmVarIdTypeList stm'
    CondElse _ stm1 stm2 -> do
      map1 <- getStmVarIdTypeList stm1
      map2 <- getStmVarIdTypeList stm2
      return $ map1 ++ map2
    While _ stm' -> getStmVarIdTypeList stm'
    ForEach t (Ident id) _ stm -> do
      let sType = typeTransform t tMap
      list <- getStmVarIdTypeList stm
      return $ (id, sType) : list
    _ -> return []
  where getItemId :: Item -> String
        getItemId item = case item of
          NonInit (Ident id) -> id
          Init (Ident id) _ -> id
    
-- | generate one variable allocation instruction, updating the environment at the same time
allocaVar :: (String, String) -> CodeGenMonad Text
allocaVar (varId, sType) = do
  (env', txt) <- gets (bindVariable sType varId)
  put env'
  return txt

-- | generate fucntion arguments initialization instructions 
generateArgsInit :: Fun -> CodeGenMonad ()
generateArgsInit fun =
  let arguments = case fun of
        FnDef _ _ args _ -> args
        PFnDef _ _ args _ -> args
  in case arguments of
    [] -> return ()
    _  -> do
      let ids = fmap (\e -> case e of
                         Argument _ (Ident id) -> id
                         PArgument _ (Ident id) -> id) arguments
          srcs = fmap (\x -> varPrefix ++ show x) [0 .. (length arguments) - 1]
      idAndTypes <- mapM (gets . findVariable) ids
      let tup = zip idAndTypes srcs
          storeTxts = fmap (\((dst, sType), src) -> llvmStore sType src dst) tup
      modify . saveText $ T.concat storeTxts

generateFunctionStms :: Fun -> CodeGenMonad ()
generateFunctionStms fun = case fun of
  FnDef _ _ _ (Block stms) -> mapM_ doGenerate stms
  PFnDef _ _ _ (Block stms) -> mapM_ doGenerate stms
  where doGenerate :: Stmt -> CodeGenMonad ()
        doGenerate stm = do
          modify (\env -> env {curStm = stm})
          generateStatement stm

-- | generate a single statement instruction
generateStatement :: Stmt -> CodeGenMonad ()
generateStatement stm = case stm of
  Empty -> return ()
  BStmt (Block stmts) -> mapM_ generateStatement stmts
  Decl t items -> generateDeclarationStmCode t items
  PDecl (Ident pid) items -> do
    t <- gets (getPointerType pid)
    generateDeclarationStmCode t items   
  Ass (Ident id) expr -> do
    (dst, sType) <- gets (findVariable id)
    src <- generateExpr sType expr
    modify . saveText $ llvmStore sType src dst
  AssElem expr1 expr2 -> case expr1 of
    ETyped t _ -> do
      Right tMap <- gets arrayTypeInfo
      src <- generateExpr "" expr2
      addr <- potentialLeftValue Nothing expr1
      let sType = typeTransform t tMap
          txt = llvmStore sType src addr
      modify (saveText txt)
    
  Incr (Ident id) -> do
    (llvmid, sType) <- gets (findVariable id)
    tmp1 <- getTmpVariable
    tmp2 <- getTmpVariable
    let loadTxt = llvmLoad sType tmp1 llvmid
        addTxt = llvmAddOp Plus sType tmp2 tmp1 "1"
        storeTxt = llvmStore sType tmp2 llvmid
    modify . saveText $ loadTxt <> addTxt <> storeTxt
  Decr (Ident id) -> do
    (llvmid, sType) <- gets (findVariable id)
    tmp1 <- getTmpVariable
    tmp2 <- getTmpVariable
    let loadTxt = llvmLoad sType tmp1 llvmid
        subTxt = llvmAddOp Plus sType tmp2 tmp1 "-1"
        storeTxt = llvmStore sType tmp2 llvmid
    modify . saveText $ loadTxt <> subTxt <> storeTxt
  ReturnE expr -> do
    retType <- gets (\x -> retType x)
    retVal <- gets (\x -> retVal x)
    src <- generateExpr retType expr
    markNeedReturn
    modify . saveText $ llvmStore retType src retVal
    modify newLabel
  ReturnV -> do
    markNeedReturn
    modify newLabel
  Sexp expr -> do
    generateExpr "void" expr
    return ()
  Cond expr stm -> do
    brVal <- generateExpr "i1" expr
    l0 <- gets curLabel
    modify newLabel
    l1 <- gets curLabel
    generateStatement stm
    l2 <- gets curLabel
    modify newLabel
    l3 <- gets curLabel
    env <- get
    let label1 = labelPrefix ++ show l1
        label3 = labelPrefix ++ show l3
        br0 = llvmConBr brVal label1 label3
        br1 = case Set.member l2 (retBlks env) of
          True -> T.empty
          _ -> llvmBr label3
    modify (appendText l0 br0)
    modify (appendText l2 br1)
  CondElse expr stm1 stm2 -> do
    val <- generateExpr "i1" expr
    l0 <- gets curLabel
    modify newLabel
    l1 <- gets curLabel
    generateStatement stm1
    l2 <- gets curLabel
    modify newLabel
    l3 <- gets curLabel
    generateStatement stm2
    l4 <- gets curLabel
    modify newLabel
    l5 <- gets curLabel
    env <- get
    let br0 = llvmConBr val (labelPrefix ++ show l1) (labelPrefix ++ show l3)
        br1 = case Set.member l2 (retBlks env) of
          True -> T.empty
          _ -> llvmBr (labelPrefix ++ show l5)
        br2 = case Set.member l4 (retBlks env) of
          True -> T.empty
          _ -> llvmBr (labelPrefix ++ show l5)
    modify (appendText l0 br0)
    modify (appendText l2 br1)
    modify (appendText l4 br2)
  While expr stm -> do
    l_0 <- gets curLabel
    modify newLabel
    l_1 <- gets curLabel
    v_0 <- generateExpr "i1" expr
    l_2 <- gets curLabel
    modify newLabel
    l_3 <- gets curLabel
    generateStatement stm
    l_4 <- gets curLabel
    modify newLabel
    l_5 <- gets curLabel
    let br_0 = llvmBr (labelPrefix ++ show l_1)
        br_1 = llvmConBr v_0 (labelPrefix ++ show l_3) (labelPrefix ++ show l_5)
    modify (appendText l_0 br_0)
    modify (appendText l_2 br_1)
    modify (appendText l_4 br_0)
  ForEach t (Ident id) expr stm -> case expr of
    ETyped arrayType _ -> do
      Right tMap <- gets arrayTypeInfo
      let arrayPointerType = typeTransform arrayType tMap 
      {- prepare to loop -}
      (dst, sType) <- gets (findVariable id)
      ptr <- generateExpr "" expr
      addr <- getTmpVariable
      size <- getTmpVariable
      limit <- getTmpVariable
      count <- getTmpVariable
      let txt1 = llvmGetElementAddress addr (init arrayPointerType) ptr ["0", "0"]
          txt2 = llvmLoad "i32" size addr
          txt3 = llvmAddOp Minus "i32" limit size "1"
          txt4 = llvmAlloc "i32" count
          txt5 = llvmStore "i32" "0" count
      modify . saveText $ txt1 <> txt2 <> txt3 <> txt4 <> txt5
      l_0 <- gets curLabel
      modify newLabel
      {- edge condition -}
      l_1 <- gets curLabel
      currentCount <- getTmpVariable
      edgeCondition <- getTmpVariable
      let txt6 = llvmLoad "i32" currentCount count
          txt7 = llvmRelOp Le "i32" edgeCondition currentCount limit
      modify . saveText $ txt6 <> txt7
      modify newLabel
      {- loop over -}
      l_2 <- gets curLabel
      elemAddr <- getTmpVariable
      elemValue <- getTmpVariable
      let txt8 = llvmGetElementAddress elemAddr (init arrayPointerType) ptr ["0", "1", currentCount]
          txt9 = llvmLoad sType elemValue elemAddr
          txt10 = llvmStore sType elemValue dst
      modify . saveText $ txt8 <> txt9 <> txt10
      generateStatement stm
      updateCount <- getTmpVariable
      let countUpdateGetTxt = llvmAddOp Plus "i32" updateCount currentCount "1"
          countUpdateStoreTxt = llvmStore "i32" updateCount count
      {- update count value -}
      modify . saveText $ countUpdateGetTxt <> countUpdateStoreTxt
      l_3 <- gets curLabel
      modify newLabel
      {- loop over -}
      l_4 <- gets curLabel
      let br_0 = llvmBr (labelPrefix ++ show l_1)
          br_1 = llvmConBr edgeCondition (labelPrefix ++ show l_2) (labelPrefix ++ show l_4)
      modify (appendText l_0 br_0)
      modify (appendText l_1 br_1)
      modify (appendText l_3 br_0)
    
-- | generate instruction for expression {TO BE CONTINUE...}
generateExpr :: String -> Expr -> CodeGenMonad String
generateExpr sType expr = case expr of
  ETyped t expr -> do
    Right tMap <- gets arrayTypeInfo
    generateExpr (typeTransform t tMap) expr
  Evar (Ident id) -> do
    dst <- getTmpVariable
    (src, sType') <- gets (findVariable id)
    let loadTxt = llvmLoad sType' dst src
    modify (saveText loadTxt)
    return dst
  ENew object -> case object of
    ArrayObj array -> do
      let (arrayType, lengthExpr) = getArrayTypeAndLengthExpr array
          (innerType, innerArray) = getArrayInside array
      tup <- getNumberAndSize lengthExpr innerType
      arrayCreation tup arrayType innerArray
    StructObj (Ident structName) -> do
      size <- gets (getStructMemorySize structName)
      ptr <- getTmpVariable
      ptr' <- getTmpVariable
      let sType = "%struct." ++ structName ++ "*"
          txt1 = llvmAllocateHeapMemory ptr "1" (show size)
          txt2 = llvmCastPointerType ptr' "i8*" ptr sType
      modify . saveText $ txt1 <> txt2
      return ptr'
  ELitInt i  -> return . show $ i
  ELitDoub d -> return . show $ d
  ELitTrue   -> return "1"
  ELitFalse  -> return "0"
  EAPP (Ident fid) exprs -> do
    case fid of
      "printString" -> case exprs of
          [EString s] -> do
            dst <- getTmpVariable
            (llvmid, len) <- gets (getStringLiteral s)
            let ptrTxt = llvmGetStrPtr dst llvmid len
                callPrntTxt = llvmCallPrntStr dst
            modify . saveText $ ptrTxt <> callPrntTxt
            return ""
      _ -> do
        Right tMap <- gets arrayTypeInfo
        argts <-  gets (findFuncParamType fid)
        let argsts = fmap (\x -> typeTransform x tMap) argts
        argVals <- mapM (\(x, y) -> generateExpr x y) (zip argsts exprs)
        case sType of
          "void" -> do
            let tups = zip argsts argVals
                callTxt = llvmCall "" fid sType tups
            modify . saveText $ callTxt
            return ""
          _ -> do
            dst <- getTmpVariable
            let tups = zip argsts argVals
                callTxt = llvmCall dst fid sType tups
            modify . saveText $ callTxt
            return dst
  EString _ -> return ""
  ENull (Ident pid) -> return "null"
  EDeref _ _  -> potentialLeftValue (Just sType) expr
  EDot _ _ -> potentialLeftValue (Just sType) expr
  EIndex _ _ -> potentialLeftValue (Just sType) expr
  Neg expr -> do
    src <- generateExpr sType expr
    dst <- getTmpVariable
    let txt = case sType of
          "i32" -> llvmIntNeg dst src
          "double" -> llvmDoubleNeg dst src
    modify . saveText $ txt
    return dst
  Not expr -> case expr of
    ELitTrue -> return "0"
    ELitFalse -> return "1"
    _ -> do
      src <- generateExpr "i1" expr
      dst <- getTmpVariable
      let txt = llvmBoolNeg dst src
      modify . saveText $ txt
      return dst
  EMul expr1 opt expr2 -> do
    val1 <- generateExpr sType expr1
    val2 <- generateExpr sType expr2
    dst <- getTmpVariable
    let txt = llvmMulOp opt sType dst val1 val2
    modify . saveText $ txt
    return dst
  EAdd expr1 opt expr2 -> do
    val1 <- generateExpr sType expr1
    val2 <- generateExpr sType expr2
    dst <- getTmpVariable
    let txt = llvmAddOp opt sType dst val1 val2
    modify . saveText $ txt
    return dst
  ERel expr1 opt expr2 -> case expr1 of
    ETyped t _ -> do
      Right tMap <- gets arrayTypeInfo
      let sType' = typeTransform t tMap
      val1 <- generateExpr sType' expr1
      val2 <- generateExpr sType' expr2
      dst <- getTmpVariable
      let txt = llvmRelOp opt sType' dst val1 val2
      modify . saveText $ txt
      return dst
  EAnd expr1 expr2 -> do
    var1 <- generateExpr sType expr1
    l0 <- gets curLabel
    modify newLabel
    l1 <- gets curLabel
    var2 <- generateExpr sType expr2
    l2 <- gets curLabel
    modify newLabel
    l3 <- gets curLabel
    dst <- getTmpVariable
    let label0 = labelPrefix ++ show l0
        label1 = labelPrefix ++ show l1
        label2 = labelPrefix ++ show l2
        label3 = labelPrefix ++ show l3
        br0 = llvmConBr var1 label1 label3
        br1 = llvmBr label3
        phiTxt = llvmPhi dst "false" label0 var2 label2 
    modify (appendText l0 br0)
    modify (appendText l2 br1)
    modify (saveText phiTxt)
    return dst
  EOr expr1 expr2 -> do
    var1 <- generateExpr sType expr1
    l0 <- gets curLabel
    modify newLabel
    l1 <- gets curLabel
    var2 <- generateExpr sType expr2
    l2 <- gets curLabel
    modify newLabel
    l3 <- gets curLabel
    dst <- getTmpVariable
    let label0 = labelPrefix ++ show l0
        label1 = labelPrefix ++ show l1
        label2 = labelPrefix ++ show l2
        label3 = labelPrefix ++ show l3
        br0 = llvmConBr var1 label3 label1
        br1 = llvmBr label3
        phiTxt = llvmPhi dst "true" label0 var2 label2
    modify (appendText l0 br0)
    modify (appendText l2 br1)
    modify (saveText phiTxt)
    return dst

generateDeclarationStmCode :: Type -> [Item] -> CodeGenMonad ()
generateDeclarationStmCode t items = do
  Right tMap <- gets arrayTypeInfo
  let sType = typeTransform t tMap
      nonInitIds = [id | NonInit (Ident id) <- items]
      initIdExprs = [(id, expr) | Init (Ident id) expr <- items]
  llvmIdTypes <- mapM (gets . findVariable) nonInitIds
  let defaultInitTxts = fmap ((defaultInit sType) . fst) llvmIdTypes
  initTxts <- mapM hdlInit initIdExprs
  modify . saveText $ (T.concat defaultInitTxts) <> (T.concat initTxts)
  where hdlInit :: (String, Expr) -> CodeGenMonad Text
        hdlInit (id, expr) = do
          (dst, sType) <- gets $ findVariable id
          src <- generateExpr sType expr
          let storeTxt = llvmStore sType src dst
          return storeTxt


-- | get a temporary variable
getTmpVariable :: CodeGenMonad String
getTmpVariable = do
  (env, tmpVar) <- gets generateTmpVar
  put env
  return tmpVar

-- | mark the current block has a return statement
markNeedReturn :: CodeGenMonad ()
markNeedReturn = do
  curLabelNum <- gets (\x -> curLabel x)
  retBlkSet <- gets (\x -> retBlks x)
  modify (\x -> x {retBlks = Set.insert curLabelNum retBlkSet})


-- | generate the last 'ret' instruction in a function
generateRetInstruction :: CodeGenMonad ()
generateRetInstruction = do
  isHeadOfBlock <- headOfBlock
  if not isHeadOfBlock
    then markNeedReturn >> modify newLabel
    else return ()
  returnLabel <- gets curLabel
  retType <- gets retType
  retVal <- gets retVal
  case retType of
    "void" -> modify . saveText $ T.pack "  ret void\n}\n"
    _ -> do
      dst <- getTmpVariable
      let loadTxt = llvmLoad retType dst retVal
          retTxt = llvmReturn retType dst
          close = T.pack "}\n"
      modify . saveText $ loadTxt <> retTxt <> close
  completeMissingRet returnLabel
  where
    completeMissingRet :: Int -> CodeGenMonad ()
    completeMissingRet num = do
      let brTxt = llvmBr (labelPrefix ++ show num)
      list <- gets (Set.toList . retBlks)
      mapM_ (modify . (\x -> appendText x brTxt)) list

headOfBlock :: CodeGenMonad Bool
headOfBlock = do
  curLabel <- gets curLabel
  Just txt <- gets ((Map.lookup curLabel) . blkM)
  let len = (length $ show curLabel) + 3
  if T.length txt == len
    then return True
    else return False

inferExprType :: Expr -> Maybe Type
inferExprType expr = case expr of
  ETyped t _ -> Just t
  _ -> Nothing

getNumberAndSize :: Expr -> Type -> CodeGenMonad (String, String)
getNumberAndSize expr t = do
  elemNumber <- generateExpr "i32" expr
  let elemTypeSize = llvmTypeSize t
  elemTotalSize <- getTmpVariable -- ^ elemSize * elemTypeSize
  structTotalSizei32 <- getTmpVariable -- ^ var1 + 4, 4 is the size of attribute 'length' of array, which has type i32 
  structTotalSizei64 <- getTmpVariable -- ^ (i64) var2, type conversion
  let txt1 = llvmMulOp Times "i32" elemTotalSize elemNumber (show elemTypeSize)
      txt2 = llvmAddOp Plus "i32" structTotalSizei32 elemTotalSize "4"
      txt3 = llvmExtendToi64 structTotalSizei64 structTotalSizei32
  modify . saveText $ txt1 <> txt2 <> txt3
  return (elemNumber, structTotalSizei64)

arrayCreation :: (String, String) -> Type -> Maybe Array -> CodeGenMonad String
arrayCreation (elemNumber, totalSize) arrayType possibleInnerArray = do
  Right tMap <- gets arrayTypeInfo
  let arrayPointerType = typeTransform arrayType tMap
  case possibleInnerArray of
    Nothing -> allocateArrayMemory elemNumber totalSize arrayPointerType
    Just innerArray -> do
      ptr <- allocateArrayMemory elemNumber totalSize arrayPointerType
      createInnerArray innerArray elemNumber arrayPointerType ptr
      return ptr
      
allocateArrayMemory :: String -> String -> String -> CodeGenMonad String
allocateArrayMemory elemNumber totalSize arrayPointerType = do
  addressi8 <- getTmpVariable -- ^ hold value1 as the address of allocated memory
  addressObj <- getTmpVariable -- ^ hold value2 which cast pointer type i8* to the actual array type pointer
  addressAttr <- getTmpVariable -- ^ hold the address of the 'length' attribute of the newly created array
  let txt1 = llvmAllocateHeapMemory addressi8 "1" totalSize
      txt2 = llvmCastPointerType addressObj "i8*" addressi8 arrayPointerType
      txt3 = llvmGetElementAddress addressAttr (init arrayPointerType) addressObj ["0", "0"]
      txt4 = llvmStore "i32" elemNumber addressAttr
  modify . saveText $ txt1 <> txt2 <> txt3 <> txt4
  return addressObj

createInnerArray :: Array -> String -> String -> String -> CodeGenMonad ()
createInnerArray innerArray elemNumber parentArrayPointerType parentArrayPointerValue = do
  Right tMap <- gets arrayTypeInfo
  let (arrayType', lengthExpr') = getArrayTypeAndLengthExpr innerArray
      (innerType', innerArray') = getArrayInside innerArray
      subarrayPointerType = typeTransform arrayType' tMap
  {- llvm block 1 -}
  l_0 <- gets curLabel
  -- ^ variable used to keep counts of the loop, initialized with value "1"
  loopCount <- getTmpVariable
  let txt1 = llvmAlloc "i32" loopCount
      txt2 = llvmStore "i32" "1" loopCount
  modify . saveText $ txt1 <> txt2
  modify newLabel
      
  {- llvm block 2 -}
  l_1 <- gets curLabel
  -- ^ current count value in the loop
  currentCount <- getTmpVariable
  -- ^ boolean value results from evaluating '1 <= elemNumber'
  edgeCondition <- getTmpVariable
  let txt3 = llvmLoad "i32" currentCount loopCount
      txt4 = llvmRelOp Le "i32" edgeCondition currentCount elemNumber
  modify . saveText $ txt3 <> txt4
  modify newLabel

  {- llvm block 3 -}
  l_2 <- gets curLabel
  -- ^ subarray generation
  tup <- getNumberAndSize lengthExpr' innerType'
  -- ^ pointer to the newly created subarray
  subarrayPointerValue <- arrayCreation tup arrayType' innerArray'
  -- ^ index used to refer to the proper address from the parent array address space,
  -- to which the value of the pointer to the newly created subarray should be assigned.
  subarrayAddressIndex <- getTmpVariable
  -- ^ pointer to the target address
  subarrayTargetAddress <- getTmpVariable
  let txt5 = llvmAddOp Minus "i32" subarrayAddressIndex currentCount "1"
      txt6 = llvmGetElementAddress subarrayTargetAddress (init parentArrayPointerType) parentArrayPointerValue ["0", "1", subarrayAddressIndex]
      txt7 = llvmStore subarrayPointerType subarrayPointerValue subarrayTargetAddress
  modify . saveText $ txt5 <> txt6 <> txt7
  -- ^ update count value: loopCount++
  countUpdateValue <- getTmpVariable
  let txt8 = llvmAddOp Plus "i32" countUpdateValue currentCount "1"
      txt9 = llvmStore "i32" countUpdateValue loopCount          
  modify . saveText $ txt8 <> txt9
  l_3 <- gets curLabel
  modify newLabel

  {- llvm block 4 -}
  l_4 <- gets curLabel
  let br_0 = llvmBr (labelPrefix ++ show l_1)
      br_1 = llvmConBr edgeCondition (labelPrefix ++ show l_2) (labelPrefix ++ show l_4)
  modify (appendText l_0 br_0)
  modify (appendText l_1 br_1)
  modify (appendText l_3 br_0)
  
potentialLeftValue :: Maybe String -> Expr -> CodeGenMonad String
potentialLeftValue mstr expr = case expr of
  ETyped _ expr' -> potentialLeftValue mstr expr'
  EDeref expr (Ident attr) -> generateCodeForStruct mstr expr attr
  EDot expr (Ident attr) -> generateCodeForStruct mstr expr attr
  EIndex expr1 expr2 -> case expr1 of
    ETyped arrayType _ -> do
      Right tMap <- gets arrayTypeInfo
      ptr <- generateExpr "" expr1
      index <- generateExpr "" expr2
      addr <- getTmpVariable
      let pointerType = typeTransform arrayType tMap
          txt1 = llvmGetElementAddress addr (init pointerType) ptr ["0", "1", index]
      case mstr of
        Nothing -> do
          modify (saveText txt1)
          return addr
        Just sType -> do
          dst <- getTmpVariable
          let txt2 = llvmLoad sType dst addr
          modify . saveText $ txt1 <> txt2
          return dst

generateCodeForStruct :: Maybe String -> Expr -> String -> CodeGenMonad String
generateCodeForStruct mstr expr attr = case expr of
  ETyped t _ -> do
    Right tMap <- gets arrayTypeInfo
    sMap <- gets structMap
    ptr <- generateExpr "" expr
    let Just structName = TCU.inquireStructName t
        Just (_, index) = getAttributeInfo structName attr sMap
        pointerType = typeTransform t tMap
    addr <- getTmpVariable
    let txt1 = llvmGetElementAddress addr (init pointerType) ptr ["0", show index]
    case mstr of
      Nothing -> do
        modify (saveText txt1)
        return addr
      Just sType -> do
        dst <- getTmpVariable
        let txt2 = llvmLoad sType dst addr
        modify . saveText $ txt1 <> txt2
        return dst
