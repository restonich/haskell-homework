module EvalDelimCC where

import Prelude hiding (abs)
import ULC
import Data.List

-- Data Types --
type Name = String
type GivenTerm = Term
type NeededTerm = Term

-- Our Magnificient term
data Term =
    TmVar    String
  | TmAbs    String    Term
  | TmApp    Term      Term
  | TmReset  Term
  | TmShift  String    Term
  | TmBinOp  BinOpSort Term Term
  | TmConst  Int
  deriving (Show)

-- Defining parsingOps with our Magnificient term for parsing's sake
parsingOps :: Ops Term
parsingOps = TermConstruction
  { var   = const   TmVar
  , abs   = const   TmAbs
  , app   = const   TmApp
  , reset = const   TmReset
  , call  = const   TmShift
  , binop = TmBinOp
  , int   = const   TmConst
  }

-- Evaluation code --

eval :: Term -> Maybe Int
eval term = 
  case evalTerm $ bNorm $ toLam term of
    (TmConst n) -> Just n
    _           -> Nothing

-- Supporting functions --  

-- We know how to work with lambda-functions, but DelimCC is little bit harder
-- So we are determining our DelimCC terms in lambda terms
toLam :: Term -> Term
toLam (TmReset a@(TmReset term))   = toLam a
--toLam (TmReset   (TmShift k term)) = toLam term
toLam (TmReset   (TmConst n))      = TmConst n
toLam (TmReset   term)             = 
  case funComp (TmAbs "x" (TmVar "x"), term) of 
    (comp, (TmShift k t)) -> TmApp (TmAbs k t) comp
    (comp, (TmReset t))   -> TmApp (toLam comp) (toLam $ TmReset t)
    (comp, (TmConst n))   -> TmApp comp (TmConst n)
toLam (TmBinOp op x y) = TmBinOp op (toLam x) (toLam y)
toLam (TmAbs v t)      = TmAbs v (toLam t)
toLam (TmApp t1 t2)    = TmApp (toLam t1) (toLam t2)
toLam x = x

-- Making magic to compose funs between shift and reset by going deep step by step in our tree       --      +
-- (\x -> a + b * x, Term)   Term : Shift | Reset | Const                                            --    /   \
--                                                                                                   --   a     *
funComp :: (Term, Term) -> (Term, Term)                                                              --        / \
funComp (TmAbs "x" (TmVar "x"), term) =                                                              --       b   Term
  case term of
    (TmBinOp op a b)  -> let (l,r) = funComp (TmBinOp op a (TmVar "x"), b) in (TmAbs "x" l, r)
    (TmApp t1 t2)     -> let (l,r) = funComp (TmApp t1 (TmVar "x"), t2)    in (TmAbs "x" l, r)
    (TmShift k t)     -> (TmAbs "x" (TmVar "x"), TmShift k t)
    (TmReset t)       -> (TmAbs "x" (TmVar "x"), TmReset t)
    (TmConst n)       -> (TmAbs "x" (TmVar "x"), TmConst n)

funComp (TmBinOp op x (TmVar "x"), term) = 
  case term of
    (TmBinOp op a b) -> let (l,r) = funComp (TmBinOp op a (TmVar "x"), b) in (TmBinOp op x l, r)
    (TmApp t1 t2)    -> let (l,r) = funComp (TmApp t1 (TmVar "x"), t2)    in (TmBinOp op x l, r)
    (TmShift k t)    -> (TmBinOp op x (TmVar "x"), TmShift k t)
    (TmReset t)      -> (TmBinOp op x (TmVar "x"), TmReset t)
    (TmConst n)      -> (TmBinOp op x (TmVar "x"), TmConst n)

funComp (TmApp x (TmVar "x"), term) = 
  case term of
    (TmBinOp op a b) -> let (l,r) = funComp (TmBinOp op a (TmVar "x"), b) in (TmApp x l, r)
    (TmApp t1 t2)    -> let (l,r) = funComp (TmApp t1 (TmVar "x"), t2)    in (TmApp x l, r)
    (TmShift k t)    -> (TmApp x (TmVar "x"), TmShift k t)
    (TmReset t)      -> (TmApp x (TmVar "x"), TmReset t)
    (TmConst n)      -> (TmApp x (TmVar "x"), TmConst n)

-- Beta-normalization of our term
bNorm :: Term -> Term
bNorm x =
  case bRed x of
    Just x0 -> bNorm x0
    Nothing -> x

-- One step of beta-reduction
bRed :: Term -> Maybe Term
bRed (TmAbs x t) = 
  case bRed t of
    Just t' -> Just (TmAbs x t')
    Nothing -> Nothing
bRed (TmApp (TmAbs x t) t2) = 
  Just (putTerm t x t2) 
bRed (TmShift k t) =
  case bRed t of
    Just t' -> Just (TmShift k t')
    Nothing -> Nothing
bRed (TmApp t1 t2) = 
  case bRed t1 of
    Just t1' -> Just (TmApp t1' t2)
    Nothing  -> case bRed t2 of
                  Just t2' -> Just (TmApp t1 t2')
                  Nothing  -> Nothing
bRed (TmReset t) =
  case bRed t of
    Just t' -> Just (TmReset t')
    Nothing -> Nothing
bRed (TmBinOp op a b) =
  case bRed a of
    Just a' -> Just (TmBinOp op a' b)
    Nothing -> case bRed b of
                Just b' -> Just (TmBinOp op a b')
                Nothing -> Nothing
bRed _ = Nothing

-- Here we're putting needed term in place of vars with corresponding name in given term 
putTerm :: GivenTerm -> Name -> NeededTerm -> Term
putTerm (TmVar x) nm repTerm
  | x == nm = repTerm
  | otherwise = TmVar x
putTerm (TmAbs x t) nm repTerm 
  | x == nm = TmAbs x t
  | (x `isFreeIn` repTerm && nm `isFreeIn` t) = let newX = x ++ "'" in
    (TmAbs newX (putTerm (putTerm t nm (TmVar newX)) nm repTerm))
  | otherwise = TmAbs nm (putTerm t nm repTerm)
putTerm (TmApp t1 t2) nm repTerm = TmApp (putTerm t1 nm repTerm) (putTerm t2 nm repTerm)
putTerm (TmShift k t) nm repTerm
  | k == nm = TmShift k t
  | (k `isFreeIn` repTerm && nm `isFreeIn` t) = let newK = k ++ "'" in
    (TmShift newK (putTerm (putTerm t k (TmVar newK)) nm repTerm))
  | otherwise = TmShift k (putTerm t nm repTerm)
putTerm (TmReset t) nm repTerm = TmReset (putTerm t nm repTerm)
putTerm (TmBinOp op a b) nm repTerm = TmBinOp op (putTerm a nm repTerm) (putTerm b nm repTerm)
putTerm x nm repTerm = x

-- Evaluation process
evalTerm :: Term -> Term
evalTerm (TmConst n) = TmConst n
evalTerm (TmBinOp Add a b) = 
  case evalTerm a of
    TmConst a' -> case evalTerm b of
                    TmConst b' -> TmConst (a' + b')
                    b'         -> TmBinOp Add (TmConst a') b'
    a'         -> TmBinOp Add a' (evalTerm b)
evalTerm (TmBinOp Mul a b) = 
    case evalTerm a of
        TmConst a' -> case evalTerm b of
                          TmConst b' -> TmConst (a' * b')
                          b'         -> TmBinOp Mul (TmConst a') b'
        a'         -> TmBinOp Mul a' (evalTerm b)
evalTerm x = x

--Checks if var of given name is free in some term
isFreeIn :: Name -> Term -> Bool
isFreeIn nm (TmVar x) = nm == x
isFreeIn nm (TmAbs x t) = x /= nm && isFreeIn nm t
isFreeIn nm (TmApp t1 t2) = isFreeIn nm t1 || isFreeIn nm t2
isFreeIn nm (TmShift k t) = k /= nm && isFreeIn nm t
isFreeIn nm (TmReset t) = isFreeIn nm t
isFreeIn nm (TmBinOp _ a b) = isFreeIn nm a || isFreeIn nm b
isFreeIn _ _ = False