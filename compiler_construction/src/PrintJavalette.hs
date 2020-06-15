{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintJavalette.
--   Generated by the BNF converter.

module PrintJavalette where

import qualified AbsJavalette
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else ' ':s)

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsJavalette.Ident where
  prt _ (AbsJavalette.Ident i) = doc (showString i)

instance Print AbsJavalette.Prog where
  prt i e = case e of
    AbsJavalette.Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print [AbsJavalette.TopDef] where
  prt = prtList

instance Print AbsJavalette.TopDef where
  prt i e = case e of
    AbsJavalette.TopDefFun fun -> prPrec i 0 (concatD [prt 0 fun])
    AbsJavalette.TopDefStruct struct -> prPrec i 0 (concatD [prt 0 struct])
    AbsJavalette.TopDefPointer pointer -> prPrec i 0 (concatD [prt 0 pointer])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsJavalette.Fun where
  prt i e = case e of
    AbsJavalette.FnDef type_ id args blk -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 blk])
    AbsJavalette.PFnDef id1 id2 args blk -> prPrec i 0 (concatD [prt 0 id1, prt 0 id2, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 blk])

instance Print AbsJavalette.Arg where
  prt i e = case e of
    AbsJavalette.Argument type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
    AbsJavalette.PArgument id1 id2 -> prPrec i 0 (concatD [prt 0 id1, prt 0 id2])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsJavalette.Arg] where
  prt = prtList

instance Print AbsJavalette.Struct where
  prt i e = case e of
    AbsJavalette.StructDef id attributes -> prPrec i 0 (concatD [doc (showString "struct"), prt 0 id, doc (showString "{"), prt 0 attributes, doc (showString "}"), doc (showString ";")])

instance Print AbsJavalette.Attribute where
  prt i e = case e of
    AbsJavalette.Attr type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString ";")])
    AbsJavalette.PAttr id1 id2 -> prPrec i 0 (concatD [prt 0 id1, prt 0 id2, doc (showString ";")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsJavalette.Attribute] where
  prt = prtList

instance Print AbsJavalette.Pointer where
  prt i e = case e of
    AbsJavalette.PointerDef id1 id2 -> prPrec i 0 (concatD [doc (showString "typedef"), doc (showString "struct"), prt 0 id1, doc (showString "*"), prt 0 id2, doc (showString ";")])

instance Print AbsJavalette.Blk where
  prt i e = case e of
    AbsJavalette.Block stmts -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print [AbsJavalette.Stmt] where
  prt = prtList

instance Print AbsJavalette.Stmt where
  prt i e = case e of
    AbsJavalette.Empty -> prPrec i 0 (concatD [doc (showString ";")])
    AbsJavalette.BStmt blk -> prPrec i 0 (concatD [prt 0 blk])
    AbsJavalette.Decl type_ items -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, doc (showString ";")])
    AbsJavalette.PDecl id items -> prPrec i 0 (concatD [prt 0 id, prt 0 items, doc (showString ";")])
    AbsJavalette.Ass id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr, doc (showString ";")])
    AbsJavalette.AssElem expr1 expr2 -> prPrec i 0 (concatD [prt 6 expr1, doc (showString "="), prt 0 expr2, doc (showString ";")])
    AbsJavalette.Incr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "++"), doc (showString ";")])
    AbsJavalette.Decr id -> prPrec i 0 (concatD [prt 0 id, doc (showString "--"), doc (showString ";")])
    AbsJavalette.ReturnE expr -> prPrec i 0 (concatD [doc (showString "return"), prt 0 expr, doc (showString ";")])
    AbsJavalette.ReturnV -> prPrec i 0 (concatD [doc (showString "return"), doc (showString ";")])
    AbsJavalette.Cond expr stmt -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsJavalette.CondElse expr stmt1 stmt2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, doc (showString "else"), prt 0 stmt2])
    AbsJavalette.While expr stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsJavalette.ForEach type_ id expr stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 type_, prt 0 id, doc (showString ":"), prt 0 expr, doc (showString ")"), prt 0 stmt])
    AbsJavalette.Sexp expr -> prPrec i 0 (concatD [prt 0 expr, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsJavalette.Item where
  prt i e = case e of
    AbsJavalette.NonInit id -> prPrec i 0 (concatD [prt 0 id])
    AbsJavalette.Init id expr -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 expr])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsJavalette.Item] where
  prt = prtList

instance Print AbsJavalette.Type where
  prt i e = case e of
    AbsJavalette.TInt -> prPrec i 0 (concatD [doc (showString "int")])
    AbsJavalette.TDoub -> prPrec i 0 (concatD [doc (showString "double")])
    AbsJavalette.TBool -> prPrec i 0 (concatD [doc (showString "boolean")])
    AbsJavalette.TVoid -> prPrec i 0 (concatD [doc (showString "void")])
    AbsJavalette.TArray type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), doc (showString "]")])
    AbsJavalette.TFun type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])
    AbsJavalette.TPointer id -> prPrec i 0 (concatD [prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsJavalette.Type] where
  prt = prtList

instance Print AbsJavalette.Expr where
  prt i e = case e of
    AbsJavalette.Evar id -> prPrec i 7 (concatD [prt 0 id])
    AbsJavalette.ENew object -> prPrec i 7 (concatD [doc (showString "new"), prt 0 object])
    AbsJavalette.ELitInt n -> prPrec i 7 (concatD [prt 0 n])
    AbsJavalette.ELitDoub d -> prPrec i 7 (concatD [prt 0 d])
    AbsJavalette.ELitTrue -> prPrec i 7 (concatD [doc (showString "true")])
    AbsJavalette.ELitFalse -> prPrec i 7 (concatD [doc (showString "false")])
    AbsJavalette.EAPP id exprs -> prPrec i 7 (concatD [prt 0 id, doc (showString "("), prt 0 exprs, doc (showString ")")])
    AbsJavalette.EString str -> prPrec i 7 (concatD [prt 0 str])
    AbsJavalette.ENull id -> prPrec i 7 (concatD [doc (showString "("), prt 0 id, doc (showString ")"), doc (showString "null")])
    AbsJavalette.EDeref expr id -> prPrec i 6 (concatD [prt 6 expr, doc (showString "->"), prt 0 id])
    AbsJavalette.EDot expr id -> prPrec i 6 (concatD [prt 6 expr, doc (showString "."), prt 0 id])
    AbsJavalette.EIndex expr1 expr2 -> prPrec i 6 (concatD [prt 6 expr1, doc (showString "["), prt 0 expr2, doc (showString "]")])
    AbsJavalette.Neg expr -> prPrec i 5 (concatD [doc (showString "-"), prt 6 expr])
    AbsJavalette.Not expr -> prPrec i 5 (concatD [doc (showString "!"), prt 6 expr])
    AbsJavalette.EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    AbsJavalette.EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    AbsJavalette.ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    AbsJavalette.EAnd expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString "&&"), prt 1 expr2])
    AbsJavalette.EOr expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString "||"), prt 0 expr2])
    AbsJavalette.ETyped type_ expr -> prPrec i 0 (concatD [prt 0 type_, prt 0 expr])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsJavalette.Expr] where
  prt = prtList

instance Print AbsJavalette.Object where
  prt i e = case e of
    AbsJavalette.ArrayObj array -> prPrec i 0 (concatD [prt 0 array])
    AbsJavalette.StructObj id -> prPrec i 0 (concatD [prt 0 id])

instance Print AbsJavalette.Array where
  prt i e = case e of
    AbsJavalette.SArray type_ expr -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), prt 0 expr, doc (showString "]")])
    AbsJavalette.MArray array expr -> prPrec i 0 (concatD [prt 0 array, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print AbsJavalette.AddOp where
  prt i e = case e of
    AbsJavalette.Plus -> prPrec i 0 (concatD [doc (showString "+")])
    AbsJavalette.Minus -> prPrec i 0 (concatD [doc (showString "-")])

instance Print AbsJavalette.MulOp where
  prt i e = case e of
    AbsJavalette.Times -> prPrec i 0 (concatD [doc (showString "*")])
    AbsJavalette.Div -> prPrec i 0 (concatD [doc (showString "/")])
    AbsJavalette.Modulo -> prPrec i 0 (concatD [doc (showString "%")])

instance Print AbsJavalette.RelOp where
  prt i e = case e of
    AbsJavalette.Lt -> prPrec i 0 (concatD [doc (showString "<")])
    AbsJavalette.Le -> prPrec i 0 (concatD [doc (showString "<=")])
    AbsJavalette.Gt -> prPrec i 0 (concatD [doc (showString ">")])
    AbsJavalette.Ge -> prPrec i 0 (concatD [doc (showString ">=")])
    AbsJavalette.Eq -> prPrec i 0 (concatD [doc (showString "==")])
    AbsJavalette.Ne -> prPrec i 0 (concatD [doc (showString "!=")])

