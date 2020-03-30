module TypeChecker
  (
    runTypeCheck
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import           AbsJavalette
import qualified AbsJavalette as AbsJ
import           Exception
import           PrintJavalette
import           TypeCheckUtil

-- | state monad for type checking, using Env as state
type TypeCheckMonad a = StateT Env IO a

runTypeCheck :: Prog -> IO ()
runTypeCheck p = do
  evalStateT (typeCheck p) emptyEnv
  return ()

-- | typeCheck method
typeCheck :: Prog -> TypeCheckMonad Prog
typeCheck (Program p) = do
  initEnv                   -- ^ init environment
  mapM_ scanFun p           -- ^ typecheck function definitions
  checkMain                 -- ^ typecheck 'main'
  mapM_ checkFun p          -- ^ typecheck each function
  return $ Program p

-- | initial environment with default primitive functions
initEnv :: TypeCheckMonad ()
initEnv = put env
  where env = Env initFunMap [] Nothing
        initFunMap = Map.fromList [ ("printInt", TFun TVoid [TInt])
                         , ("printDouble", TFun TVoid [TDoub])
                         , ("printString", TFun TVoid [])
                         -- ^ a special case, actually function 'printString' accepts a string literal, but we don't have a string type in our language, so we will treat it specially
                         , ("readInt", TFun TInt [])
                         , ("readDouble", TFun TDoub [])]


scanFun :: Fun -> TypeCheckMonad ()
scanFun (FnDef t (Ident fid) as _) = do
  env <- get
  let funMap = funs env
      v = Map.lookup fid funMap
  case v of
    Nothing -> do
      let ts = extractType as
          b  = triggerEval ts
      case b of
        True -> let t' = TFun t ts
                    nm = Map.insert fid t' funMap
                in put $ env {funs = nm}
    _ -> throwTce $ "Duplicate definition of function: " ++ fid ++ "!"
  where extractType as = case as of
          [] -> []
          (Argument t (Ident varId)) : xs -> case t of
            TVoid -> throwTce $ "Illegal type 'void' for variable: " ++ varId ++ "!\n" ++
              "In function signature: " ++ fid ++ ""
            _    -> t : (extractType xs)
        triggerEval ts = and [if i == TVoid then False else True | i <- ts]
          
checkMain :: TypeCheckMonad ()
checkMain = do
  env <- get
  let funMap = funs env
      mfm    = Map.lookup "main" funMap
  case mfm of
    Nothing -> throwTce $ "Missing function 'main'!"
    Just (TFun t ts) -> case t of
      TInt -> case ts of
        [] -> return ()
        _  -> throwTce $ "Type error of function 'main': parameter list must be empty!"
      _   -> throwTce $ "Type error of function 'main': return type must be 'int'!"

checkFun :: Fun -> TypeCheckMonad ()
checkFun (FnDef funRt (Ident fid) as (Block bs)) = do
  modify enterBlock
  modify $ setCurrentF (FnDef funRt (Ident fid) as (Block bs))
  mapM_ hdlArgVar as
  mapM_ hdlStmt bs
  modify unsetCurrentF
  modify exitBlock
  where
    hdlArgVar :: Arg -> TypeCheckMonad ()
    hdlArgVar (Argument t (Ident id)) = checkThenAddVar Nothing id t

    hdlStmtBlock :: Stmt -> TypeCheckMonad ()
    hdlStmtBlock stmt = do
      modify enterBlock
      hdlStmt stmt
      modify exitBlock

    hdlStmtListBlock :: [Stmt] -> TypeCheckMonad ()
    hdlStmtListBlock stmtls = do
      modify enterBlock
      mapM_ hdlStmt stmtls
      modify exitBlock
    
    hdlStmt :: Stmt -> TypeCheckMonad ()
    hdlStmt stmt = case stmt of
      Empty -> return ()
      
      BStmt (Block ss) -> hdlStmtListBlock ss

      Decl t items -> do
        mapM_ (hdlItem stmt t) items

      Ass (Ident id) expr -> do
        mt <- gets $ findVarType id
        case mt of
          Nothing -> throwTce $ "Unknown variable: " ++ id ++ "!\nStatement: " ++
            printTree stmt ++ "In function: " ++ fid
          Just t -> do
            t' <- infer stmt expr
            if t' == t
              then return ()
              else throwTce $ "Type mismatch, expected: " ++ showType t ++ ", actual: " ++
                     showType t' ++ "!\nStatement:\n" ++ printTree stmt ++
                     "In Function: " ++ fid

      Incr (Ident id) -> do
        mt <- gets $ findVarType id
        case mt of
          Nothing -> throwTce $ "Unknown variable: " ++ id ++ "!\nStatement:\n" ++
            printTree stmt ++ "In function: " ++ fid
          Just t -> case t of
            TInt -> return ()
            _    -> throwTce $ "Type mismatch, '++' can only be applied on int variable!\n"
                      ++ "Statement:\n" ++ printTree stmt ++ "In function: " ++ fid

      Decr (Ident id) -> do
        mt <- gets $ findVarType id
        case mt of
          Nothing -> throwTce $ "Unknown variable: " ++ id ++ "!\nStatement:\n" ++
            printTree stmt ++ "In function: " ++ fid
          Just t -> case t of
            TInt -> return ()
            _    -> throwTce $ "Type mismatch, '++' can only be applied on int variable!\n"
                      ++ "Statement:\n" ++ printTree stmt ++ "In function: " ++ fid
            
      ReturnE expr -> do
        case funRt of
          TVoid -> throwTce $ "Function return type mismatch, returning value in " ++
            "a 'void' typed function!\n" ++ "Statement:\n" ++ printTree stmt
            ++ "In function: " ++ fid
          _ -> do
            t' <- infer stmt expr
            if funRt == t'
              then return ()
              else throwTce $ "Function return type mismatch, expected: " ++ showType funRt
                     ++ ", actual: " ++ showType t' ++ "!\nStatement:\n" ++ printTree stmt
                     ++ "In function: " ++ fid

      ReturnV -> case funRt of
        TVoid -> return ()
        _     -> throwTce $ "Function return type mismatch, a concrete value is expected!\n"
                   ++ "In function: " ++ fid

      Cond expr stmt' -> do
        exprt <- infer stmt expr
        case exprt of
          TBool -> hdlStmtBlock stmt'
          _     -> throwTce $ "Type mismatch, conditional expression must be of type boolean"
                     ++ "!\nStatement:\n" ++ printTree stmt ++ "In function: " ++ fid

      CondElse expr stmt1 stmt2 -> do
        exprt <- infer stmt expr
        case exprt of
          TBool -> mapM_ hdlStmtBlock [stmt1, stmt2]
          _ -> throwTce $ "Type mismatch, conditional expression must be of type boolean"
                 ++ "!\nStatement:\n" ++ printTree stmt ++ "In function: " ++ fid

      While expr stmt' -> do
        exprt <- infer stmt expr
        case exprt of
          TBool -> hdlStmtBlock stmt'
          _ -> throwTce $ "Type mismatch, condition expression must be of type boolean!\n"
                 ++ "Statement:\n" ++ printTree stmt

      Sexp expr -> case expr of
        EAPP _ _ -> do
          exprt <- infer stmt expr
          case exprt of
            TVoid -> return ()
            _ -> throwTce $ "Type mismatch, expression as statement only limits to 'void' "
                   ++ "function invocation!\nStatement:\n" ++ printTree stmt ++
                   "In function: " ++ fid
        _ -> throwTce $ "Type mismatch, expression as statement only limits to 'void' "
               ++ "function invocation!\nStatement:\n" ++ printTree stmt ++ "In function: "
               ++ fid

    hdlItem :: Stmt -> Type -> Item -> TypeCheckMonad ()
    hdlItem stmt t it = case it of
      NonInit (Ident id)   -> checkThenAddVar (Just stmt) id t
      Init (Ident id) exp -> do
        t' <- infer stmt exp
        if t == t'
          then checkThenAddVar (Just stmt) id t
          else throwTce $ "Type mismatch on variable: " ++ id ++ "!\nExpected: " ++
                 showType t ++ ", actual: " ++ showType t' ++ "!\nStatement:\n" ++ printTree
                 stmt ++ "In function: " ++ fid
                 
    checkThenAddVar :: Maybe Stmt -> String -> Type -> TypeCheckMonad ()
    checkThenAddVar mstm id t = do
      let stmStr = case mstm of
            Nothing -> "!\nIn function: " ++ fid
            Just stm -> "!\nStatement:\n" ++ printTree stm ++ "In function: " ++ fid
      case t of
        TVoid -> throwTce $ "Illegal type 'void' for variable: " ++ id ++ stmStr
        _    -> do
          env <- get
          let b = existInTopBlock id env
          case b of
            True -> throwTce $ "Duplicate variable declaration, variable: " ++ id ++ stmStr
            _    -> modify (addVarType id t)

infer :: Stmt -> Expr -> TypeCheckMonad Type
infer stm exp = do
  env <- get
  curfName <- gets getCurrentFunName
  case exp of
    Evar (Ident id) -> do
      let mt = findVarType id env
      case mt of
        Nothing -> throwTce $ "Unknown variable: " ++ id ++ "!\nStatement:\n" ++ printTree
          stm ++ "In function: " ++ curfName
        Just t  -> return t
        
    ELitInt _  -> return TInt
    
    ELitDoub _ -> return TDoub
    
    ELitTrue   -> return TBool
    
    ELitFalse  -> return TBool
    
    EAPP (Ident fid) exprs -> do
      let mft = findFunType fid env
      case mft of
        Nothing -> throwTce $ "Unknown function call: " ++ fid ++ "!\nStatement:\n" ++
          printTree stm ++ "In function: " ++ curfName
        Just (TFun rt ats) -> case fid of
          "printString" -> case exprs of
             [EString ps] -> return TVoid
             _            -> throwTce $ "Type mismatch, only one string literal is allowed" ++
                               " as the parameter of function 'printString'!\nIn function: "
                               ++ curfName
          _ -> do
            b <- matchArgTypes fid ats exprs
            case b of
              True -> return rt

    EString str -> throwTce $ "String literal should only be used as input parameter for " ++
                     "function 'printString'!"

    Neg expr -> do
      t <- infer stm expr
      if t /= TInt && t /= TDoub
        then throwTce $ "'-' can only be applied on int or double!\n" ++
               "In expression: " ++ printTree (Neg expr) ++ "!\nStatement:\n" ++ printTree stm
               ++ "In function: " ++ curfName
        else return t

    Not expr -> do
      t <- infer stm expr
      if t /= TBool
        then throwTce $ "'!' can only be applied on boolean!\nStatement:\n" ++ printTree stm ++
               "In function: " ++ curfName
        else return TBool

    EMul expr1 op expr2
      | (op == Times || op == Div) ->
          matchOptrType "'*', '/'" (Set.fromList [TInt, TDoub]) (EMul expr1 op expr2)
            expr1 expr2
      | otherwise -> matchOptrType "'%'" (Set.fromList [TInt]) (EMul expr1 op expr2)
                      expr1 expr2
      
    EAdd expr1 op expr2 -> matchOptrType "'+', '-'" (Set.fromList [TInt, TDoub])
                             (EAdd expr1 op expr2) expr1 expr2

    ERel expr1 op expr2
      | Set.member op (Set.fromList [AbsJ.Lt, AbsJ.Le, AbsJ.Gt, AbsJ.Ge]) -> do
          matchOptrType "'<', '<=', '>', '>='" (Set.fromList [TInt, TDoub])
            (ERel expr1 op expr2) expr1 expr2
          return TBool
      | otherwise -> do
          matchOptrType "'==', '!='" (Set.fromList [TInt, TDoub, TBool])
            (ERel expr1 op expr2) expr1 expr2
          return TBool

    EAnd expr1 expr2 -> matchOptrType "'&&'" (Set.fromList [TBool]) (EAnd expr1 expr2)
                          expr1 expr2

    EOr expr1 expr2  -> matchOptrType "'||'" (Set.fromList [TBool]) (EAnd expr1 expr2)
                          expr1 expr2

  where
    matchArgTypes :: String -> [Type] -> [Expr] -> TypeCheckMonad Bool
    matchArgTypes id ats exprs = do
      curfName <- gets getCurrentFunName
      let l1 = length ats
          l2 = length exprs
      if l1 /= l2
        then throwTce $ "Parameter size mismatch, expected: " ++ show l1
               ++ ", actual: " ++ show l2 ++ ", when invoking function: " ++ id ++ "!\n" ++
               "In function: " ++ curfName
        else do let xs = zip [1..] $ zip ats exprs
                rs <- mapM (matchArgTypes' id) xs
                return $ and rs

    matchArgTypes' :: String -> (Integer, (Type, Expr)) -> TypeCheckMonad Bool
    matchArgTypes' id (i, (t, exp)) = do
      curfName <- gets getCurrentFunName
      t' <- infer stm exp
      if t == t'
        then return True
        else throwTce $ "Type mismatch, expected: " ++ showType t ++
               ", actual: " ++ showType t' ++ ", at the " ++ show i ++
               "th parameter of function " ++ id ++ "!\nStatement:\n" ++ printTree stm ++
               "In function: " ++ curfName    
      
    matchOptrType :: String -> Set Type -> Expr -> Expr -> Expr -> TypeCheckMonad Type
    matchOptrType optStr availableTypes expr expr1 expr2 = do
      curfName <- gets getCurrentFunName
      t1 <- infer stm expr1
      t2 <- infer stm expr2
      if t1 /= t2
        then throwTce $ "Type mismatch, expr1 has type: " ++ showType t1
               ++ ", expr2 has type: " ++ showType t2 ++ "!\nStatement:\n" ++ printTree stm ++
               "In function: " ++ curfName
        else if Set.notMember t1 availableTypes
                then throwTce $ optStr ++ " can only be applied on type" ++
                       printTypeSet availableTypes  ++ "!\nStatement:\n" ++ printTree stm
                       ++ "In function: " ++ curfName
                else return t1
        
    printTypeSet :: Set Type -> String
    printTypeSet st = let s = Set.foldr f "" st in init s
      where f t s = case t of
              TInt     -> " int," ++ s
              TDoub    -> " double," ++ s
              TBool    -> " boolean," ++ s
              TVoid    -> " void," ++ s
              TFun _ _ -> s 
