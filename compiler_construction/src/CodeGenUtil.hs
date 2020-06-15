module CodeGenUtil where
import           Control.Monad.State.Strict

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe

import           PrintJavalette
import           AbsJavalette
import           TypeCheckUtil (StructInfo(..))
import qualified TypeCheckUtil as TCU
import           Exception

-- | environment data type used for llvm code generation.
data Env = Env {
  -- ^ map for function info
  funM     :: Map String Type,
  -- ^ map for string info
  strM     :: Map String (String, Int),
  -- ^ map for variables
  varM     :: Map String (String, String),
  -- ^ number of next variable or label
  next     :: Int,
  -- ^ return type of current function
  retType  :: String,
  -- ^ temporary llvm variable for the function return value
  retVal   :: String,
  -- ^ current block label
  curLabel :: Int,
  -- ^ map for storing instruction blocks for a particular function
  blkM     :: Map Int Text,
  -- ^ set of number of blocks that need to be supplied with the number of
  -- the last block to finish its own instructions
  retBlks  :: Set Int,
  -- ^ map for storing struct information
  structMap :: Map String StructInfo,
  -- ^ map for storing pointer information
  pointerMap :: Map String String,
  -- ^ a structor used to hold program array inforamtion. This field will be initialized with
  -- a Left value, then the left set will be converted to a Right map, which records each
  -- arry type to its proper llvm representation
  arrayTypeInfo :: Either (Set Type) (Map Type String),
  -- ^ only used for debugging
  curStm :: Stmt
} deriving Show

-- | internal reserved function names
reservedFunctionNames = Set.fromList ["calloc", "free"]

-- | collision between names of user defined funcitons and internal reserved functions will
-- be resolved by renaming the former by appending a suffix '_v0'
renameWhenClash :: String -> String
renameWhenClash id = case Set.member id reservedFunctionNames of
  True -> id ++ "_v0"
  _ -> id

-- | initial environment
emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty 0 "" "" 0 Map.empty Set.empty Map.empty Map.empty (Left Set.empty) Empty

initFuncMap = Map.fromList
  [("printInt", TFun TVoid [TInt])
    , ("printDouble", TFun TVoid [TDoub])
    , ("printString", TFun TVoid [])
    , ("readInt", TFun TInt [])
    , ("readDouble", TFun TDoub [])]

-- | environment data type for unreachable statement filtering
data UnreachableEnv = UnreachableEnv {pass :: Bool}
-- | environment data type for variable renaming
data RenamingEnv = RenamingEnv {
  index  :: Int,
  nameM :: [Map String String]
}
-- | environment data type for expression type annotation
data AnnotateEnv = AnnotateEnv {tMap :: Map String Type, sMap :: Map String StructInfo, pMap :: Map String String} deriving Show

-- | monad support for unreachable statement filter process
type UnreachableMonad a = State UnreachableEnv a
-- | monad support for variable renaming process
type RenameMonad a = State RenamingEnv a
-- | monad support for expression type annotation process
type AnnotateMonad a = State AnnotateEnv a

emptyRenameEnv :: RenamingEnv
emptyRenameEnv = RenamingEnv 0 []

emptyFilterEnv :: UnreachableEnv
emptyFilterEnv = UnreachableEnv True

varPrefix = "%v"
labelPrefix = "%L"

-- | generalised method for searching a value in a stack of lookup table
findBindingVal :: (Ord k, Eq k) => k -> [Map k v] -> Maybe v
findBindingVal k maplist = case maplist of
  [] -> Nothing
  (x:xs) -> case Map.lookup k x of
    Nothing -> findBindingVal k xs
    v -> v 
         
-- | get rid of empty statements of a function
filterEmptyStm :: Fun -> Fun
filterEmptyStm fun = case fun of
  FnDef t fid args (Block stms) -> let stms' = doFilter stms in FnDef t fid args (Block stms')
  PFnDef id fid args (Block stms) -> let stms' = doFilter stms in PFnDef id fid args (Block stms')
  where doFilter :: [Stmt] -> [Stmt]
        doFilter stms = catMaybes (fmap filterEmpty stms)

filterEmpty :: Stmt -> Maybe Stmt
filterEmpty stmt = case stmt of
  Empty -> Nothing
  BStmt (Block bs) -> case catMaybes (fmap filterEmpty bs) of
    []  -> Nothing
    bs' -> Just (BStmt (Block bs'))
  Cond expr s -> case filterEmpty s of
    Nothing -> Just (Cond expr Empty)
    Just s' -> Just (Cond expr s')
  CondElse expr s1 s2 ->
    let ms1 = filterEmpty s1
        ms2 = filterEmpty s2
        s1' = case ms1 of
          Nothing -> Empty
          Just x  -> x
        s2' = case ms2 of
          Nothing -> Empty
          Just x  -> x
    in Just (CondElse expr s1' s2')
  While expr s -> case filterEmpty s of
    Nothing -> Just (While expr Empty)
    Just s' -> Just (While expr s')
  ForEach t id e s -> case filterEmpty s of
    Nothing -> Nothing
    Just s' -> Just (ForEach t id e s')
  _ -> Just stmt

filterRedundantBoolLit :: Fun -> Fun
filterRedundantBoolLit fun = case fun of
  FnDef t id args (Block stms) -> let stms' = doFilter stms in FnDef t id args (Block stms')
  PFnDef id fid args (Block stms) -> let stms' = doFilter stms in PFnDef id fid args (Block stms')
  where doFilter :: [Stmt] -> [Stmt]
        doFilter stms = fmap reduce stms 

reduce :: Stmt -> Stmt
reduce stm = case stm of
  BStmt (Block stmts) -> BStmt (Block (fmap reduce stmts))
  Decl t items -> Decl t (fmap reduceItem items)
  PDecl p items -> PDecl p (fmap reduceItem items)
  Ass id expr -> Ass id (reduceExpr expr)
  AssElem expr1 expr2 -> AssElem (reduceExpr expr1) (reduceExpr expr2)
  ReturnE expr -> ReturnE (reduceExpr expr)
  Cond expr stmt -> Cond (reduceExpr expr) (reduce stmt)
  CondElse expr stm1 stm2 -> CondElse (reduceExpr expr) (reduce stm1) (reduce stm2)
  While expr stm -> While (reduceExpr expr) (reduce stm)
  ForEach t id expr stm -> ForEach t id (reduceExpr expr) (reduce stm)
  _ -> stm

reduceItem :: Item -> Item
reduceItem item = case item of
  Init id expr -> Init id (reduceExpr expr)
  _ -> item

reduceArray :: Array -> Array
reduceArray array = case array of
  SArray t expr -> SArray t (reduceExpr expr)
  MArray innerArray expr ->
    let innerArray' = reduceArray innerArray
        expr' = reduceExpr expr
    in MArray innerArray' expr'

reduceExpr :: Expr -> Expr
reduceExpr expr = case expr of
  ENew object -> case object of
    ArrayObj array -> let array' = reduceArray array in ENew (ArrayObj array')
    _ -> expr
  EAPP id exprs -> EAPP id (fmap reduceExpr exprs)
  EDot expr' id -> EDot (reduceExpr expr') id
  EDeref expr id -> EDeref (reduceExpr expr) id
  EIndex expr1 expr2 -> EIndex (reduceExpr expr1) (reduceExpr expr2)
  Neg expr -> Neg (reduceExpr expr)
  Not expr -> Not (reduceExpr expr)
  EMul expr1 opt expr2 -> EMul (reduceExpr expr1) opt (reduceExpr expr2)
  EAdd expr1 opt expr2 -> EAdd (reduceExpr expr1) opt (reduceExpr expr2)
  ERel expr1 opt expr2 ->
    let expr1' = reduceExpr expr1
        expr2' = reduceExpr expr2
    in ERel expr1' opt expr2'
  EAnd expr1 expr2 -> case expr1 of
    ELitTrue -> reduceExpr expr2
    ELitFalse -> ELitFalse
    _ -> EAnd (reduceExpr expr1) (reduceExpr expr2)
  EOr expr1 expr2 -> case expr1 of
    ELitTrue -> ELitTrue
    ELitFalse -> reduceExpr expr2
    _ -> EOr (reduceExpr expr1) (reduceExpr expr2)
  _ -> expr


filterUnreachableStm :: Fun -> Fun
filterUnreachableStm fun = evalState (unreachableStmFilter fun) emptyFilterEnv

-- | filter unreachable statements of a function
unreachableStmFilter :: Fun -> UnreachableMonad Fun
unreachableStmFilter fun = case fun of
  FnDef t id args (Block stms) -> do
    mstms <- mapM dofilter stms
    let stms' = catMaybes mstms
    return $ FnDef t id args (Block stms')
  PFnDef id fid args (Block stms) -> do
    mstms <- mapM dofilter stms
    let stms' = catMaybes mstms
    return $ PFnDef id fid args (Block stms')
  where dofilter :: Stmt -> UnreachableMonad (Maybe Stmt)
        dofilter stm = do
          tag <- gets pass
          case tag of
            False -> return Nothing
            _ -> case stm of
              BStmt (Block stms) -> do
                mstms <- mapM dofilter stms
                let stms' = catMaybes mstms
                case stms' of
                  [] -> return Nothing
                  _  -> return $ Just (BStmt (Block stms'))
              ReturnE _ -> do
                modify (\x -> x {pass = False})
                return $ Just stm
              ReturnV -> do
                modify (\x -> x {pass = False})
                return $ Just ReturnV
              Cond expr stm -> case expr of
                ELitFalse -> return Nothing
                ELitTrue -> do
                  mstm <- dofilter stm
                  case mstm of
                    Nothing -> return Nothing
                    Just stm' -> return $ Just (Cond expr stm')
                _ -> do
                  mstm <- dofilter stm
                  modify (\x -> x {pass = True})
                  case mstm of
                    Nothing -> return $ Just (Cond expr Empty)
                    Just stm' -> return $ Just (Cond expr stm')
              CondElse expr stm1 stm2 -> case expr of
                ELitTrue -> dofilter (Cond ELitTrue stm1)
                ELitFalse -> dofilter (Cond ELitTrue stm2)
                _ -> do
                  mstm1 <- dofilter stm1
                  tag1 <- gets pass
                  modify (\x -> x {pass = True})
                  mstm2 <- dofilter stm2
                  tag2 <- gets pass
                  modify (\x -> x {pass = tag1 || tag2})
                  case mstm1 of
                    Nothing -> case mstm2 of
                      Nothing -> return $ Just (CondElse expr Empty Empty)
                      Just stm2' -> return $ Just (CondElse expr Empty stm2')
                    Just stm1' -> case mstm2 of
                      Nothing -> return $ Just (CondElse expr stm1' Empty)
                      Just stm2' -> return $ Just (CondElse expr stm1' stm2')
              While expr stm -> case expr of
                ELitFalse -> return Nothing
                ELitTrue -> do
                  mstm <- dofilter stm
                  modify (\x -> x {pass = False})
                  case mstm of
                    Nothing -> return Nothing
                    Just stm' -> return $ Just (While ELitTrue stm')
                _ -> do
                  mstm <- dofilter stm
                  modify (\x -> x {pass = True})
                  case mstm of
                    Nothing -> return $ Just (While expr Empty)
                    Just stm' -> return $ Just (While expr stm')
              ForEach t id expr stm -> do
                stm' <- dofilter stm
                case stm' of
                  Nothing -> return Nothing
                  Just stm'' -> return $ Just (ForEach t id expr stm'')
              _ -> return (Just stm)     

getRenamedFunction :: Fun -> Fun
getRenamedFunction f = evalState (renamingVariable f) emptyRenameEnv

-- | rename original varialbes so that all of them are distinct. With this we can have only
-- one level of lookup table to map javalette variable to its llvm counterpart!
renamingVariable :: Fun -> RenameMonad Fun
renamingVariable fun = case fun of
  FnDef t id args (Block stms) -> do
    modify enterBlock
    args' <- mapM renameArg args
    stms' <- mapM renameStm stms
    return $ FnDef t id args' (Block stms')
  PFnDef id fid args (Block stms) -> do
    modify enterBlock
    args' <- mapM renameArg args
    stms' <- mapM renameStm stms
    return $ PFnDef id fid args' (Block stms')
  where enterBlock :: RenamingEnv -> RenamingEnv
        enterBlock env =
          let stack = nameM env
              stack' = Map.empty : stack
          in env {nameM = stack'}

        bind :: String -> RenameMonad String
        bind oldName = do
          idx <- gets index
          stack <- gets nameM
          let topM = head stack
              newName = "v" ++ show idx
              topM' = Map.insert oldName newName topM
              idx' = idx + 1
              stack' = topM' : (drop 1 stack)
          modify (\x -> x {index = idx', nameM = stack'})
          return newName

        find :: String -> RenameMonad String
        find oldName = do
          stack <- gets nameM
          let Just newName = findBindingVal oldName stack
          return newName

        exitBlock :: RenamingEnv -> RenamingEnv
        exitBlock env = let stack = nameM env in env {nameM = drop 1 stack}

        renameArg :: Arg -> RenameMonad Arg
        renameArg arg = case arg of
          Argument t (Ident id) -> do
            id' <- bind id
            return (Argument t (Ident id'))
          PArgument pid (Ident id) -> do
            id' <- bind id
            return (PArgument pid (Ident id'))
        
        renameItem :: Item -> RenameMonad Item
        renameItem item = case item of
          NonInit (Ident id) -> do
            newName <- bind id
            return $ NonInit (Ident newName)
          Init (Ident id) expr -> do
            expr' <- renameExpr expr
            newName <- bind id
            return $ Init (Ident newName) expr'

        renameArray :: Array -> RenameMonad Array
        renameArray array = case array of
          SArray t expr -> do
            expr' <- renameExpr expr
            return $ SArray t expr'
          MArray innerArray expr -> do
            innerArray' <- renameArray innerArray
            expr' <- renameExpr expr
            return $ MArray innerArray' expr'
        
        renameStm :: Stmt -> RenameMonad Stmt
        renameStm stm = case stm of
          Empty -> return Empty
          BStmt (Block stms) -> do
            modify enterBlock
            stms' <- mapM renameStm stms
            modify exitBlock
            return $ BStmt (Block stms')
          Decl t items -> do
            items' <- mapM renameItem items
            return $ Decl t items'
          PDecl pid items -> do
            items' <- mapM renameItem items
            return $ PDecl pid items'
          Ass (Ident id) expr -> do
            newName <- find id
            expr' <- renameExpr expr
            return $ Ass (Ident newName) expr'
          AssElem expr1 expr2 -> do
            expr1' <- renameExpr expr1
            expr2' <- renameExpr expr2
            return $ AssElem expr1' expr2'
          Incr (Ident id) -> do
            newName <- find id
            return $ Incr (Ident newName)
          Decr (Ident id) -> do
            newName <- find id
            return $ Decr (Ident newName)
          ReturnE expr -> do
            expr'<- renameExpr expr
            return $ ReturnE expr'
          ReturnV -> return ReturnV
          Cond expr stm -> do
            expr' <- renameExpr expr
            modify enterBlock
            stm' <- renameStm stm
            modify exitBlock
            return $ Cond expr' stm'
          CondElse expr stm1 stm2 -> do
            expr' <- renameExpr expr
            modify enterBlock
            stm1' <- renameStm stm1
            modify exitBlock
            modify enterBlock
            stm2' <- renameStm stm2
            modify exitBlock
            return $ CondElse expr' stm1' stm2'
          While expr stm -> do
            expr' <- renameExpr expr
            modify enterBlock
            stm' <- renameStm stm
            modify exitBlock
            return $ While expr' stm'
          ForEach t (Ident varId) expr stm -> do
            expr' <- renameExpr expr
            modify enterBlock
            newName <- bind varId
            case stm of
              BStmt (Block stms) -> do
                stms' <- mapM renameStm stms
                modify exitBlock
                return $ ForEach t (Ident newName) expr' (BStmt (Block stms'))
              _ -> do
                stm' <- renameStm stm
                modify exitBlock
                return $ ForEach t (Ident newName) expr' stm'
          Sexp expr -> do
            expr' <- renameExpr expr
            return $ Sexp expr'

        renameExpr :: Expr -> RenameMonad Expr
        renameExpr expr = case expr of
          Evar (Ident id) -> do
            newName <- find id
            return $ Evar (Ident newName)
          ENew object -> case object of
            ArrayObj array -> do
              array' <- renameArray array
              return $ ENew (ArrayObj array')
            _ -> return expr
          ELitInt _ -> return expr
          ELitDoub _ -> return expr
          ELitTrue -> return ELitTrue
          ELitFalse -> return ELitFalse
          EAPP (Ident appid) exprs -> do
            let appid' = renameWhenClash appid
            exprs' <- mapM renameExpr exprs
            return $ EAPP (Ident appid') exprs'
          EString _ -> return expr
          ENull _ -> return expr
          EDeref expr id -> do
            expr' <- renameExpr expr
            return $ EDeref expr' id
          EDot expr id -> do
            expr' <- renameExpr expr
            return $ EDot expr' id
          EIndex expr1 expr2 -> do
            expr1' <- renameExpr expr1
            expr2' <- renameExpr expr2
            return $ EIndex expr1' expr2'
          Neg expr -> do
            expr' <- renameExpr expr
            return $ Neg expr'
          Not expr -> do
            expr' <- renameExpr expr
            return $ Not expr'
          EMul expr1 opt expr2 -> do
            expr1' <- renameExpr expr1
            expr2' <- renameExpr expr2
            return $ EMul expr1' opt expr2'
          EAdd expr1 opt expr2 -> do
            expr1' <- renameExpr expr1
            expr2' <- renameExpr expr2
            return $ EAdd expr1' opt expr2'
          ERel expr1 opt expr2 -> do
            expr1' <- renameExpr expr1
            expr2' <- renameExpr expr2
            return $ ERel expr1' opt expr2'
          EAnd expr1 expr2 -> do
            expr1' <- renameExpr expr1
            expr2' <- renameExpr expr2
            return $ EAnd expr1' expr2'
          EOr expr1 expr2 -> do
            expr1' <- renameExpr expr1
            expr2' <- renameExpr expr2
            return $ EOr expr1' expr2'

getAnnotatedFunction :: Map String Type -> Map String StructInfo -> Map String String -> Fun -> Fun
getAnnotatedFunction funcMap sMap pMap f = evalState (annotateExpression f) (AnnotateEnv funcMap sMap pMap)

-- | annotate function expression with type information
annotateExpression :: Fun -> AnnotateMonad Fun
annotateExpression fun = case fun of
  FnDef t id args (Block stms) -> do
    mapM_ registerArg args
    stms' <- mapM annotateStm stms
    return $ FnDef t id args (Block stms')
  PFnDef pid fid args (Block stms) -> do
    mapM_ registerArg args
    stms' <- mapM annotateStm stms
    return $ PFnDef pid fid args (Block stms')
  where registerArg :: Arg -> AnnotateMonad ()
        registerArg arg = case arg of
          Argument t (Ident id) -> bindVarType id t
          PArgument (Ident pid) (Ident id) -> bindPointerType id pid
          
        bindVarType :: String -> Type -> AnnotateMonad ()
        bindVarType id t = do
          typeMap <- gets tMap
          let typeMap' = Map.insert id t typeMap
          modify (\x -> x {tMap = typeMap'})

        bindPointerType :: String -> String -> AnnotateMonad ()
        bindPointerType id pid = do
          typeMap <- gets tMap
          ptrMap <- gets pMap
          let Just structId = Map.lookup pid ptrMap
              pointerType = TPointer (Ident structId)
          let typeMap' = Map.insert id pointerType typeMap
          modify (\env -> env {tMap = typeMap'})

        findVarType :: String -> AnnotateMonad Type
        findVarType id = do
          typeMap <- gets tMap
          let Just t = Map.lookup id typeMap
          case t of
            TFun t' _ -> return t'
            _ -> return t

        annotateItem :: Type -> Item -> AnnotateMonad Item
        annotateItem t item = case item of
          NonInit (Ident id) -> do
            bindVarType id t
            return item
          Init (Ident id) expr -> do
            expr' <- annotateExpr expr
            bindVarType id t
            return $ Init (Ident id) expr'

        annotateArray :: Array -> AnnotateMonad (Type, Array)
        annotateArray array = case array of
          SArray t expr -> do
            expr' <- annotateExpr expr
            return (TArray t, SArray t expr')
          MArray innerArray expr -> do
            (innerType, innerArray') <- annotateArray innerArray
            expr' <- annotateExpr expr
            return (TArray innerType, MArray innerArray' expr')

        annotateStm :: Stmt -> AnnotateMonad Stmt
        annotateStm stm = case stm of
          Empty -> return Empty
          BStmt (Block stms) -> do
            stms' <- mapM annotateStm stms
            return $ BStmt (Block stms')
          Decl t items -> do
            items' <- mapM (annotateItem t) items
            return $ Decl t items'
          PDecl (Ident pid) items -> do
            ptrMap <- gets pMap
            let Just structName = Map.lookup pid ptrMap
                t = TPointer (Ident structName)
            items' <- mapM (annotateItem t) items
            return $ PDecl (Ident pid) items'
          Ass id expr -> do
            expr' <- annotateExpr expr
            return $ Ass id expr'
          AssElem expr1 expr2 -> do
            expr1' <- annotateExpr expr1
            expr2' <- annotateExpr expr2
            return $ AssElem expr1' expr2'
          Incr id -> return stm
          Decr id -> return stm
          ReturnE expr -> do
            expr' <- annotateExpr expr
            return $ ReturnE expr'
          ReturnV -> return ReturnV
          Cond expr stm -> do
            expr' <- annotateExpr expr
            stm' <- annotateStm stm
            return $ Cond expr' stm'
          CondElse expr stm1 stm2 -> do
            expr' <- annotateExpr expr
            stm1' <- annotateStm stm1
            stm2' <- annotateStm stm2
            return $ CondElse expr' stm1' stm2'
          While expr stm -> do
            expr' <- annotateExpr expr
            stm'  <- annotateStm stm
            return $ While expr' stm'
          ForEach t (Ident id) expr stm -> do
            bindVarType id t
            expr' <- annotateExpr expr
            stm'  <- annotateStm stm
            return $ ForEach t (Ident id) expr' stm'
          Sexp expr -> do
            expr' <- annotateExpr expr
            return $ Sexp expr'

        annotateExpr :: Expr -> AnnotateMonad Expr
        annotateExpr expr = case expr of
          Evar (Ident id) -> do
            t <- findVarType id
            return $ ETyped t expr
          ENew object -> case object of
            ArrayObj array -> do
              (t, array') <- annotateArray array
              return $ ETyped t (ENew (ArrayObj array'))
            StructObj id -> return $ ETyped (TPointer id) expr
          ELitInt _ -> return $ ETyped TInt expr
          ELitDoub _ -> return $ ETyped TDoub expr
          ELitTrue -> return $ ETyped TBool expr
          ELitFalse -> return $ ETyped TBool expr
          EAPP (Ident appid) exprs -> do
            appType <- findVarType appid
            exprs' <- mapM annotateExpr exprs
            return $ ETyped appType (EAPP (Ident appid) exprs')
          EString _ -> return expr
          ENull (Ident pid) -> do
            ptrMap <- gets pMap
            let Just structName = Map.lookup pid ptrMap
            return $ ETyped (TPointer (Ident structName)) expr
          EDeref expr (Ident attrName) -> do
            expr' <- annotateExpr expr
            case expr' of
              ETyped (TPointer (Ident structName)) _ -> do
                structMap <- gets sMap
                let mi = getAttributeInfo structName attrName structMap
                case mi of
                  Just (t, _) -> return $ ETyped t (EDeref expr' (Ident attrName))
          EDot expr (Ident attrName) -> do
            expr' <- annotateExpr expr
            case expr' of
              ETyped (TArray _) _ -> do
                structMap <- gets sMap
                let mi = getAttributeInfo TCU.namingArrayAsStruct attrName structMap
                case mi of
                  Just (t, _) -> do
                    return $ ETyped t (EDot expr' (Ident attrName))
          EIndex expr1 expr2 -> do
            expr1' <- annotateExpr expr1
            expr2' <- annotateExpr expr2
            case expr1' of
              ETyped (TArray t) _ -> return $ ETyped t (EIndex expr1' expr2')
          Neg expr -> do
            expr' <- annotateExpr expr
            case expr' of
              ETyped t _ -> return $ ETyped t (Neg expr')
          Not expr -> do
            expr' <- annotateExpr expr
            return $ ETyped TBool (Not expr')
          EMul expr1 opt expr2 -> do
            expr1' <- annotateExpr expr1
            expr2' <- annotateExpr expr2
            case expr1' of
              ETyped t _ -> return $ ETyped t (EMul expr1' opt expr2')
          EAdd expr1 opt expr2 -> do
            expr1' <- annotateExpr expr1
            expr2' <- annotateExpr expr2
            case expr1' of
              ETyped t _ -> return $ ETyped t (EAdd expr1' opt expr2')
          ERel expr1 opt expr2 -> do
            expr1' <- annotateExpr expr1
            expr2' <- annotateExpr expr2
            return $ ETyped TBool (ERel expr1' opt expr2')
          EAnd expr1 expr2 -> do
            expr1' <- annotateExpr expr1
            expr2' <- annotateExpr expr2
            return $ ETyped TBool (EAnd expr1' expr2')
          EOr expr1 expr2 -> do
            expr1' <- annotateExpr expr1
            expr2' <- annotateExpr expr2
            return $ ETyped TBool (EOr expr1' expr2')

reverseArrayOrder :: Fun -> Fun
reverseArrayOrder fun = case fun of
  FnDef t id args (Block stms) -> let stms' = fmap reverseStmArrayOrder stms in FnDef t id args (Block stms')
  PFnDef pid fid args (Block stms) -> let stms' = fmap reverseStmArrayOrder stms in PFnDef pid fid args (Block stms')
  where
    reverseStmArrayOrder :: Stmt -> Stmt
    reverseStmArrayOrder stm = case stm of
      Empty -> Empty
      BStmt (Block stms) -> let stms' = fmap reverseStmArrayOrder stms in BStmt (Block stms')
      Decl t items -> let items' = fmap reverseItemArrayOrder items in Decl t items'
      PDecl pid items -> let items' = fmap reverseItemArrayOrder items in PDecl pid items'
      Ass id expr -> Ass id (reverseExprArrayOrder expr)
      AssElem expr1 expr2 ->
        let expr1' = reverseExprArrayOrder expr1
            expr2' = reverseExprArrayOrder expr2
        in AssElem expr1' expr2'
      Incr _ -> stm
      Decr _ -> stm
      ReturnE expr -> ReturnE (reverseExprArrayOrder expr)
      ReturnV -> ReturnV
      Cond expr stm ->
        let expr' = reverseExprArrayOrder expr
            stm'  = reverseStmArrayOrder stm
        in Cond expr' stm'
      CondElse expr1 stm1 stm2 ->
        let expr1' = reverseExprArrayOrder expr1
            stm1'= reverseStmArrayOrder stm1
            stm2' = reverseStmArrayOrder stm2
        in CondElse expr1' stm1' stm2'
      While expr stm ->
        let expr' = reverseExprArrayOrder expr
            stm' = reverseStmArrayOrder stm
        in While expr' stm'
      ForEach t id expr stm ->
        let expr' = reverseExprArrayOrder expr
            stm' = reverseStmArrayOrder stm
        in ForEach t id expr' stm'
      Sexp expr -> Sexp (reverseExprArrayOrder expr)

    reverseItemArrayOrder :: Item -> Item
    reverseItemArrayOrder item = case item of
      NonInit _ -> item
      Init id expr -> Init id (reverseExprArrayOrder expr)

    reverseExprArrayOrder :: Expr -> Expr
    reverseExprArrayOrder expr = case expr of
      ENew object -> case object of
        ArrayObj array -> let array' = arrayExprReverse array in ENew (ArrayObj array')
        _ -> expr
      EAPP id exprs -> let exprs' = fmap reverseExprArrayOrder exprs in EAPP id exprs'
      EDeref expr id -> let expr' = reverseExprArrayOrder expr in EDeref expr' id
      EDot expr id -> let expr' = reverseExprArrayOrder expr in EDot expr' id
      EIndex expr1 expr2 ->
        let expr1' = reverseExprArrayOrder expr1
            expr2' = reverseExprArrayOrder expr2
        in EIndex expr1' expr2'
      Neg expr -> Neg (reverseExprArrayOrder expr)
      Not expr -> Not (reverseExprArrayOrder expr)
      EMul expr1 opt expr2 ->
        let expr1' = reverseExprArrayOrder expr1
            expr2' = reverseExprArrayOrder expr2
        in EMul expr1' opt expr2'
      EAdd expr1 opt expr2 ->
        let expr1' = reverseExprArrayOrder expr1
            expr2' = reverseExprArrayOrder expr2
        in EAdd expr1' opt expr2'
      ERel expr1 opt expr2 ->
        let expr1' = reverseExprArrayOrder expr1
            expr2' = reverseExprArrayOrder expr2
        in ERel expr1' opt expr2'
      EAnd expr1 expr2 ->
        let expr1' = reverseExprArrayOrder expr1
            expr2' = reverseExprArrayOrder expr2
        in EAnd expr1' expr2'
      EOr expr1 expr2 ->
        let expr1' = reverseExprArrayOrder expr1
            expr2' = reverseExprArrayOrder expr2
        in EOr expr1' expr2'
      _ -> expr

arrayExprReverse :: Array -> Array
arrayExprReverse array =
  let baseType = arrayBaseType array
      exprlist = arrayExprList array
      Right result = foldr reassemble (Left baseType) exprlist
  in result
  where
    reassemble :: Expr -> Either Type Array -> Either Type Array
    reassemble expr ei = case ei of
      Left t -> Right (SArray t expr)
      Right array -> Right (MArray array expr)
  
arrayBaseType :: Array -> Type
arrayBaseType array = case array of
  SArray t _ -> t
  MArray innerArray _ -> arrayBaseType innerArray

arrayExprList :: Array -> [Expr]
arrayExprList array = case array of
  SArray _ expr -> [expr]
  MArray innerArray expr' -> (arrayExprList innerArray) ++ [expr']

-- | get struct attribute type
getAttributeInfo :: String -> String -> Map String StructInfo -> Maybe (Type, Int)
getAttributeInfo structName attrName structMap = case Map.lookup structName structMap of
  Nothing -> Nothing
  Just struct -> let aMap = attrMap struct in case Map.lookup attrName aMap of
    Nothing -> Nothing
    x -> x

-- | share the function type information and struct information handed from type checking
informationShare :: TCU.Env -> Env -> Env
informationShare checkEnv env =
  let funMap = TCU.funs checkEnv
      sMap = TCU.structMap checkEnv
      pMap = TCU.pointerMap checkEnv  
      arrayTypeSet = TCU.arrayTypes checkEnv
      funMap' = Map.mapKeys renameWhenClash funMap
      oldMap = funM env
      newMap = Map.union oldMap funMap'
  in env {funM = newMap, structMap = sMap, pointerMap = pMap, arrayTypeInfo = Left arrayTypeSet}

-- | refresh values of the environment indenpendent of different functions
envRefresh :: Env -> Env
envRefresh env = env {varM = Map.empty, next = 0, retType = "", retVal = "", curLabel = 0, blkM = Map.empty, retBlks = Set.empty}

-- | transform a javalette type into llvm type representation. Basic type return value,
-- complex type, like array, struct, returns pointer.
typeTransform :: Type -> Map Type String -> String
typeTransform t tMap = case t of
  TInt      -> "i32"
  TDoub     -> "double"
  TBool     -> "i1"
  TVoid     -> "void"
  TArray _  -> let Just x = Map.lookup t tMap in x ++ "*"
  TPointer (Ident s) -> "%struct." ++ s ++ "*"

-- | javalette primitive functions
primitiveFunctions :: Set String
primitiveFunctions = Set.fromList ["printInt", "printDouble", "printString", "readInt", "readDouble"]

llvmFuncForm :: String -> Text
llvmFuncForm str =
  let str' = case str of
        "printInt"    -> "declare void @printInt(i32)"
        "printDouble" -> "declare void @printDouble(double)"
        "printString" -> "declare void @printString(i8*)"
        "readInt"     -> "declare i32 @readInt()"
        "readDouble"  -> "declare double @readDouble()"
        "calloc"      -> "declare i8* @calloc(i64, i64)"
        "free"        -> "declare void @free(i8*)"
  in T.pack str'

-- | find all the external function invocations in a program (a list of functions)
progPrimitiveFunc :: [Fun] -> [String]
progPrimitiveFunc fs = Set.toList . Set.fromList $ concat (fmap extract fs)
  where extract :: Fun -> [String]
        extract fun = case fun of
          FnDef _ _ _ (Block stms) -> doExtract stms
          PFnDef _ _ _ (Block stms) -> doExtract stms

        doExtract :: [Stmt] -> [String]
        doExtract stms = concat (fmap stmPrimitiveFunc stms)
          
        -- | find all the invocations of external functions in a statement
        stmPrimitiveFunc :: Stmt -> [String]
        stmPrimitiveFunc stm = case stm of
          BStmt (Block stmts) -> concat (fmap stmPrimitiveFunc stmts)
          Decl _ items -> concat (fmap itemPrimitiveFunc items)
          PDecl _ items -> concat (fmap itemPrimitiveFunc items)
          Ass _ expr -> exprPrimitiveFunc expr
          AssElem expr1 expr2 ->
            let ss1 = exprPrimitiveFunc expr1
                ss2 = exprPrimitiveFunc expr2
            in ss1 ++ ss2
          ReturnE expr -> exprPrimitiveFunc expr
          Cond expr stm ->
            let ss1 = exprPrimitiveFunc expr
                ss2 = stmPrimitiveFunc stm
            in ss1 ++ ss2
          CondElse expr stm1 stm2 ->
            let ss0 = exprPrimitiveFunc expr
                ss1 = stmPrimitiveFunc stm1
                ss2 = stmPrimitiveFunc stm2
            in ss0 ++ ss1 ++ ss2
          While expr stm ->
            let ss1 = exprPrimitiveFunc expr
                ss2 = stmPrimitiveFunc stm
            in ss1 ++ ss2
          ForEach _ _ expr stm ->
            let ss1 = exprPrimitiveFunc expr
                ss2 = stmPrimitiveFunc stm
            in ss1 ++ ss2
          Sexp expr -> exprPrimitiveFunc expr
          _ -> []

        itemPrimitiveFunc :: Item -> [String]
        itemPrimitiveFunc item = case item of
          Init _ expr -> exprPrimitiveFunc expr
          _ -> []

        arrayPrimitiveFunc :: Array -> [String]
        arrayPrimitiveFunc array = case array of
          SArray _ expr -> exprPrimitiveFunc expr
          MArray innerArray expr ->
            let ss = arrayPrimitiveFunc innerArray
                ss' = exprPrimitiveFunc expr
            in ss ++ ss'

        exprPrimitiveFunc :: Expr -> [String]
        exprPrimitiveFunc expr = case expr of
          ENew object -> case object of
            ArrayObj array ->
              let ss = arrayPrimitiveFunc array
              in ["calloc", "free"] ++ ss
            _ -> ["calloc", "free"]
          EAPP (Ident id) exprs ->
            let l1 = case Set.member id primitiveFunctions of
                  True -> [id]
                  _    -> []
            in l1 ++ (concat $ fmap exprPrimitiveFunc exprs)
          EDot expr _ -> exprPrimitiveFunc expr
          EDeref expr _ -> exprPrimitiveFunc expr
          EIndex expr1 expr2 -> (exprPrimitiveFunc expr1) ++ (exprPrimitiveFunc expr2)
          Neg expr -> exprPrimitiveFunc expr
          Not expr -> exprPrimitiveFunc expr
          EMul expr1 _ expr2 -> (exprPrimitiveFunc expr1) ++ (exprPrimitiveFunc expr2)
          EAdd expr1 _ expr2 -> (exprPrimitiveFunc expr1) ++ (exprPrimitiveFunc expr2)
          ERel expr1 _ expr2 -> (exprPrimitiveFunc expr1) ++ (exprPrimitiveFunc expr2)
          EAnd expr1 expr2 -> (exprPrimitiveFunc expr1) ++ (exprPrimitiveFunc expr2)
          EOr expr1 expr2 -> (exprPrimitiveFunc expr1) ++ (exprPrimitiveFunc expr2)
          _ -> []

-- | map javalette string to llvm string declaration
llvmStringForm :: (Int, String) -> (Text, (String, (String, Int)))
llvmStringForm (i, str) =
  let len     = (length str) + 1
      str'    = substituteNewlineChar str
      llvmid  = "@.str." ++ show i
      llvmStr = llvmid ++ " = private constant [" ++ show len ++ " x i8] c\"" ++ str' ++ "\""
  in  (T.pack llvmStr, (str, (llvmid, len)))

-- | substitute character '\n' to "\\0A"
substituteNewlineChar :: String -> String
substituteNewlineChar str = T.unpack $ foldr f (T.pack "\\00") str
  where f :: Char -> Text -> Text
        f ch t = if ch == '\n'
          then T.append (T.pack "\\0A") t
          else T.cons ch t

-- | find all strings in a program (or a list of functions)
progString :: [Fun] -> [String]
progString fs = Set.toList . Set.fromList $ concat (fmap f fs)
  where f :: Fun -> [String]
        f fun = case fun of
          FnDef _ _ _ (Block stms) -> concat (fmap stmString stms)
          PFnDef _ _ _ (Block stms) -> concat (fmap stmString stms)

-- | find all strings in a statement
stmString :: Stmt -> [String]
stmString stm = case stm of
  Sexp exp -> case exp of
    EAPP (Ident "printString") [EString str] -> [str]
    _ -> []
  BStmt (Block stms) -> concat (fmap stmString stms)
  Cond _ stm -> stmString stm
  CondElse _ stm1 stm2 -> (stmString stm1) ++ (stmString stm2)
  While _ stm -> stmString stm
  ForEach _ _ _ stm -> stmString stm
  _ -> []

-- | save string literals information into the environment
saveStringLiteral :: Map String (String, Int) -> Env -> Env
saveStringLiteral map env = env {strM = map}

-- | get string literal information
getStringLiteral :: String -> Env -> (String, Int)
getStringLiteral str env =
  let Just v = Map.lookup str (strM env) in v

-- | llvm function head instruction
llvmFunctionHead :: Map String Type -> Map Type String -> Fun -> (Text, String)
llvmFunctionHead funMap tMap fun = case fun of
  FnDef _ (Ident id) args _ -> let Just ft = Map.lookup id funMap in generateHead ft tMap id
  PFnDef _ (Ident id) args _ -> let Just ft = Map.lookup id funMap in generateHead ft tMap id
  where
    generateHead :: Type -> Map Type String -> String -> (Text, String)
    generateHead funcType tMap fid = case funcType of
      TFun rt argts ->
        let rtStr = typeTransform rt tMap
            t1 = T.pack $ "define " ++ rtStr ++ " @" ++ fid ++ "("
        in case argts of
          [] -> (t1 <> (T.pack "){"), rtStr)
          _ -> let seq = fmap (\x -> varPrefix ++ show x) [0, 1 ..]
                   argts' = fmap (\x -> typeTransform x tMap) argts
                   tup = zip seq argts'
                   t2 = fmap makeArgText tup
                   t3 = T.init (T.concat t2)
               in (t1 <> t3 <> (T.pack "){"), rtStr)                  
        
    makeArgText :: (String, String) -> Text
    makeArgText (seq, sType) = T.pack $ sType ++ " " ++ seq ++ ","

-- | add prefix and suffix to a llvm IR instruction
instructFormat :: String -> String
instructFormat src = "  " ++ src ++ "\n"

-- | allocate space for a llvm data type and return the pointer
llvmAlloc :: String -> String -> Text
llvmAlloc sType dst = T.pack . instructFormat $ dst ++ " = alloca " ++ sType

llvmStore :: String -> String -> String -> Text
llvmStore sType src dst = T.pack . instructFormat $
  "store " ++ sType ++ " " ++ src ++ ", " ++ sType ++ "* " ++ dst

-- | load the value of type 'sType' in the 'src' location to a variable 'dst'
llvmLoad :: String -> String -> String -> Text
llvmLoad sType dst src = T.pack . instructFormat $
  dst ++ " = load " ++ sType ++ ", " ++ sType ++ "* " ++ src 

-- | generate reserved address for return value if function's return type is
-- not 'void'
generateReturnVal :: Type -> Env -> (Env, Text)
generateReturnVal t env = case t of
  TVoid -> (env, T.empty)
  _ -> let Right tMap = arrayTypeInfo env
           sType = typeTransform t tMap
           num = next env
           dst = varPrefix ++ show num
           env' = env {next = num + 1, retVal = dst}
           txt = llvmAlloc sType dst
       in (env', txt)

-- | generate a temporary llvm variable
generateTmpVar :: Env -> (Env, String)
generateTmpVar env =
  let num  = next env
      env' = env {next = num + 1}
      tmp  = varPrefix ++ show num
  in (env', tmp)
  
-- | return a new label and a new environment
-- every time we create a new label, we update the following values in the environment:
-- next -> incr next, for obvious reason
-- curLabel -> update the new label as the current label
-- retLabel -> update the new label as the possible last label where a return instruction resides in
-- txtBlk 
newLabel :: Env -> Env
newLabel env =
  let num = next env
      labelTxt = T.pack $ "\nL" ++ show num ++ ":\n"
      blkMap = Map.insert num labelTxt (blkM env)
  in env {next = num + 1, curLabel = num, blkM = blkMap}

saveText :: Text -> Env -> Env
saveText newTxt env =
  let currentLabelNum = curLabel env
      blkMap = blkM env
      Just currentTxt = Map.lookup currentLabelNum blkMap
      blkMap' = Map.insert currentLabelNum (currentTxt <> newTxt) blkMap
  in env {blkM = blkMap'}

appendText :: Int -> Text -> Env -> Env
appendText blkNum text env =
  let blkMap = blkM env
      Just blkTxt = Map.lookup blkNum blkMap
      blkTxt' = blkTxt <> text
      blkMap' = Map.insert blkNum blkTxt' blkMap
  in env {blkM = blkMap'}
  
-- | set 'next' attribute to the given value
setEnvNext :: Int -> Env -> Env
setEnvNext i env = env {next = i}

-- | bind a javalette variable to a llvm variable
bindVariable :: String -> String -> Env -> (Env, Text)
bindVariable sType varId env =
  let num = next env
      dst = varPrefix ++ show num
      varMap = Map.insert varId (dst, sType) (varM env)
      env' = env {next = num + 1, varM = varMap}
      txt = llvmAlloc sType dst
  in (env', txt)
      
-- | find llvm variable info related with a javalette variable
findVariable :: String -> Env -> (String, String)
findVariable id env =
  let Just v = Map.lookup id (varM env) in v

-- | default variable initializetion instruction
defaultInit :: String -> String -> Text
defaultInit sType dst =
  let src = case sType of
        "i32"    -> "0"
        "double" -> "0.0"
        "i1"     -> "false"
        _        -> ""
  in if src /= "" then llvmStore sType src dst else T.empty

-- | return function parameter type list
findFuncParamType :: String -> Env -> [Type]
findFuncParamType fid env = let Just (TFun _ argts) = Map.lookup fid (funM env) in argts
  
llvmReturn :: String -> String -> Text
llvmReturn sType src = T.pack . instructFormat $ "ret " ++ sType ++ " " ++ src

-- | get string literal start address
llvmGetStrPtr :: String -> String -> Int -> Text
llvmGetStrPtr dst ref len = T.pack . instructFormat $
  dst ++ " = getelementptr [" ++ show len ++ " x i8], [" ++
    show len ++ " x i8]* " ++ ref ++ ", i32 0, i32 0"
    
-- | 'printString' function invocation instruction
llvmCallPrntStr :: String -> Text
llvmCallPrntStr dst = T.pack . instructFormat $ "call void @printString(i8* " ++ dst ++ ")" 

-- | general function invocation instruction
llvmCall :: String -> String -> String -> [(String, String)] -> Text
llvmCall dst fid sType tups =
  let fid' = "@" ++ fid
      argStr = case tups of
        [] -> ""
        _  -> init $ foldr (\(t, v) s -> t ++ " " ++ v ++ "," ++ s) "" tups
      s2 = sType ++ " " ++ fid' ++ "(" ++ argStr ++ ")"
      s1 = case dst of
        "" -> "call "
        _  -> dst ++ " = call "
  in T.pack . instructFormat $ s1 ++ s2
       
-- | integer negation instruction
llvmIntNeg :: String -> String -> Text
llvmIntNeg dst src = T.pack . instructFormat $ dst ++ " = sub i32 0, " ++ src

-- | double negation instruction
llvmDoubleNeg :: String -> String -> Text
llvmDoubleNeg dst src = T.pack . instructFormat $ dst ++ " = fsub double -0.0, " ++ src

-- | boolean negation insruction
llvmBoolNeg :: String -> String -> Text
llvmBoolNeg dst src = T.pack . instructFormat $ dst ++ " = xor i1 " ++ src ++ ", true"

-- | multiplication / division / remainder instruction
llvmMulOp :: MulOp -> String -> String -> String -> String -> Text
llvmMulOp opt sType dst v1 v2 =
  let x = case opt of
        Times -> case sType of
          "i32" -> " = mul i32 "
          "double" -> " = fmul double "
        Div -> case sType of
          "i32" -> " = sdiv i32 "
          "double" -> " = fdiv double "              
        Modulo -> " = srem i32 "
  in T.pack . instructFormat $ dst ++ x ++ v1 ++ ", " ++ v2

-- | addition / minus instruction
llvmAddOp :: AddOp -> String -> String -> String -> String -> Text
llvmAddOp opt sType dst v1 v2 =
  let x = case opt of
        Plus -> case sType of
          "i32" -> " = add i32 "
          "double" -> " = fadd double "
        Minus -> case sType of
          "i32" -> " = sub i32 "
          "double" -> " = fsub double "
  in T.pack . instructFormat $ dst ++ x ++ v1 ++ ", " ++ v2

-- | relational operation instruction
llvmRelOp :: RelOp -> String -> String -> String -> String -> Text
llvmRelOp opt sType dst v1 v2 =
  let x = case opt of
        Lt -> case sType of
          "i32" -> " = icmp slt i32 "
          "double" -> " = fcmp olt double "
        Le -> case sType of
          "i32" -> " = icmp sle i32 "
          "double" -> " = fcmp ole double "
        Gt -> case sType of
          "i32" -> " = icmp sgt i32 "
          "double" -> " = fcmp ogt double "
        Ge -> case sType of
          "i32" -> " = icmp sge i32 "
          "double" -> " = fcmp oge double "
        Eq -> case sType of
          "i32" -> " = icmp eq i32 "
          "i1"  -> " = icmp eq i1 "
          "double" -> " = fcmp oeq double "
          '%': xs -> " = icmp eq " ++ sType ++ " " 
        Ne -> case sType of
          "i32" -> " = icmp ne i32 "
          "i1"  -> " = icmp ne i1"
          "double" -> " = fcmp one double "
          '%': xs -> " = icmp ne " ++ sType ++ " "
  in T.pack . instructFormat $ dst ++ x ++ v1 ++ ", " ++ v2

-- | llvm conditional branch 
llvmConBr :: String -> String -> String -> Text
llvmConBr v l1 l2 = T.pack . instructFormat $
  "br i1 " ++ v ++ ", label " ++ l1 ++ ", label " ++ l2

-- | llvm unconditional branch
llvmBr :: String -> Text
llvmBr l = T.pack . instructFormat $ "br label " ++ l

-- | llvm 'phi' instruction
llvmPhi :: String -> String -> String -> String -> String -> Text
llvmPhi dst v1 b1 v2 b2 = T.pack . instructFormat $
  dst ++ " = phi i1 [ " ++ v1 ++ ", " ++ b1 ++ " ], [ " ++ v2 ++ ", " ++ b2 ++ " ]"

llvmExtendToi64 :: String -> String -> Text
llvmExtendToi64 dst src = T.pack . instructFormat $
  dst ++ " = sext i32 " ++ src ++ " to i64"

llvmTypeSize :: Type -> Int
llvmTypeSize t = case t of
  TInt  -> 4
  TDoub -> 8
  TBool -> 1
  _     -> 8 -- ^ accounts for pointer type in llvm

llvmAllocateHeapMemory :: String -> String -> String -> Text
llvmAllocateHeapMemory dst numSize typeSize = T.pack . instructFormat $
  dst ++ " = call noalias i8* @calloc(i64 " ++ numSize ++ ", i64 " ++ typeSize ++ ")"

llvmCastPointerType :: String -> String -> String -> String -> Text
llvmCastPointerType dst fromType value toType = T.pack . instructFormat $
  dst ++ " = bitcast " ++ fromType ++ " " ++ value ++ " to " ++ toType
  
llvmGetElementAddress :: String -> String -> String -> [String] -> Text
llvmGetElementAddress dst sType pointerValue indices =
  let str1 = dst ++ " = getelementptr " ++ sType ++ ", " ++ sType ++ "* " ++ pointerValue ++ ","
      str2 = init . concat $ fmap (\i -> " i32 " ++ i ++ ",") indices
  in T.pack . instructFormat $ str1 ++ str2

-- | array types in llvm representation
llvmArrayTypes :: Set Type -> (Text, Map Type String)
llvmArrayTypes arraySet = let arrayList = Set.toList arraySet in snd . (foldr convert (1, (T.empty, Map.empty))) $ arrayList
  where convert :: Type -> (Int, (Text, Map Type String)) -> (Int, (Text, Map Type String))
        convert t (num, (txt, tMap)) = case Map.lookup t tMap of
          Just _ -> (num, (txt, tMap))
          _ -> case t of
            TArray innerType ->
              let (num', (txt', tMap')) = case innerType of
                    TArray _ -> convert innerType (num, (txt, tMap))
                    _ -> (num, (txt, tMap))
                  typeString = "%array" ++ show num'
                  innerTypeString = typeTransform innerType tMap'
                  newText = declareArrayTypeText typeString innerTypeString txt'
                  newMap = Map.insert t typeString tMap'
              in (num' + 1, (newText, newMap))

        declareArrayTypeText :: String -> String -> Text -> Text
        declareArrayTypeText s1 s2 txt =
          let textToAppend = T.pack $ s1 ++ " = type { i32, [ 0 x " ++ s2 ++ " ] }\n"
          in txt <> textToAppend

llvmStructs :: Map Type String -> [StructInfo] -> Text
llvmStructs arrayTypeMap structs = T.concat (fmap (generateStructType arrayTypeMap) structs)

generateStructType :: Map Type String -> StructInfo -> Text
generateStructType aMap struct =
  let structName = name struct in
    if structName == TCU.namingArrayAsStruct
      then T.empty
      else let s1 = "%struct." ++ structName ++ " = type {"
               attrlist = Map.elems (attrMap struct)
               attrlist' = List.sortOn snd attrlist
               typelist = fmap fst attrlist'
               s2 = init (concat (fmap (\x -> " " ++ (typeTransform x aMap) ++ ",") typelist))
           in T.pack $ s1 ++ s2 ++ " }\n"

getArrayInside :: Array -> (Type, Maybe Array)
getArrayInside array = case array of
  SArray t _ -> (t, Nothing)
  MArray innerArray _ -> let (t', _) = getArrayInside innerArray in
    (TArray t', Just innerArray)

getArrayTypeAndLengthExpr :: Array -> (Type, Expr)
getArrayTypeAndLengthExpr array = case array of
  SArray t expr -> (TArray t, expr)
  MArray innerArray expr -> let (t', _) = getArrayTypeAndLengthExpr innerArray in
    (TArray t', expr)


getPointerType :: String -> Env -> Type
getPointerType pid env =
  let pMap = pointerMap env
      Just s = Map.lookup pid pMap
  in TPointer (Ident s)

getStructMemorySize :: String -> Env -> Int
getStructMemorySize structId env =
  let sMap = structMap env
      Just sInfo = Map.lookup structId sMap
      attrTypelist = fmap fst (Map.elems (attrMap sInfo))
  in calculateStructSize attrTypelist

calculateStructSize :: [Type] -> Int
calculateStructSize attrTypes = foldr cal 0 attrTypes
  where cal :: Type -> Int -> Int
        cal t num = case t of
          TInt       -> num + 4
          TDoub      -> num + 8
          TBool      -> num + 1
          TArray _   -> num + 8
          TPointer _ -> num + 8
