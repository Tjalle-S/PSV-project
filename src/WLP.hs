module WLP (wlpTree, makeWLPs) where

import Expr ( Expr (..), ExprF(..) )
import Data.Fix ( Fix (..) )
import GCLParser.GCLDatatype ( BinOp(..) )
import TreeBuilder ( ExecStmt(..), ExecTree(..), ExecTreeF(..), replaceVar )

import Data.Functor.Foldable ( Recursive (cata), Corecursive (embed) )

import Util ( optionalError )

-- | Create a list of all WLP's of a program, one for each path (lazily).
makeWLPs :: Expr -> ExecTree -> [Expr]
makeWLPs q = cata f
  where
    f (NodeF s ts)     = map (wlpStmt s) (concat ts)
    f (TerminationF s) = [wlpStmt s q]

wlpTree :: ExecTree -> Expr -> Expr
wlpTree (Node s ts) q = wlpStmt s wlpChildrenCombined
  where
    wlpChildren = map (`wlpTree` q) ts
    wlpChildrenCombined = foldr1 (BinopExpr And) wlpChildren
wlpTree (Termination s) q = wlpStmt s q



wlpStmt :: ExecStmt -> Expr -> Expr
wlpStmt ESkip = id
wlpStmt (EAssert e1) = BinopExpr And e1
wlpStmt (EAssume e1) = BinopExpr Implication e1
wlpStmt (EAssign s e) = cata f--foldExpr (defaultAlgebra {var=replaceVar s e})
  where
    f :: ExprF Expr -> Expr
    f e'@(VarF s' t) = replaceVar s e s' t--foldExpr (defaultAlgebra {var=replaceVar s e})
    f e = embed e
wlpStmt (EAAssign s i e) = cata f --foldExpr (defaultAlgebra {var=replaceVar s (RepBy (Var s) i e)})
  where
    f :: ExprF Expr -> Expr
    f e'@(VarF s' t) = replaceVar s' (RepBy (Var s t) i (embed e')) s t--foldExpr (defaultAlgebra {var=replaceVar s e})
    f e = embed e
wlpStmt (EDrefAssign s e) = optionalError -- Reference types.
wlpStmt EBlock = error "TODO"

--replace :: String -> Expr -> Expr -> Expr
--replace s e1 e2 = cata f e2
--  where
--    f :: ExprF Expr -> Expr
--    f (Var s' t) =  replaceVar s' e1 s--foldExpr (defaultAlgebra {var=replaceVar s e})
--    f e' = Fix e'
--
--replaceVar :: [Char] -> Expr -> [Char] -> Expr
--replaceVar s1 e s2 | s1==s2 = e
--                   | otherwise = Fix $ Var s2 (PType PTInt) --This is temporary

-- replace s1 e1 (Var s2) | s1==s2 = e1
--                        | otherwise = Var s2
-- replace _ _ e2@(LitI _) = e2
-- replace _ _ e2@(LitB _) = e2
-- replace _ _ e2@(LitNull) = e2
-- replace s1 e1 (Parens e2) = Parens (replace s1 e1 e2)
-- replace s1 e1 (ArrayElem e2 e3) = ArrayElem (replace s1 e1 e2) (replace s1 e1 e3)
-- replace s1 e1 (OpNeg e2) = OpNeg (replace s1 e1 e2)
-- replace s1 e1 (BinopExpr op e2 e3) = BinopExpr op (replace s1 e1 e2) (replace s1 e1 e3)
-- replace s1 e1 (Forall s e2) = Forall s (replace s1 e1 e2)
-- replace s1 e1 (Exists s e2) = Exists s (replace s1 e1 e2)
-- replace s1 e1 (SizeOf e2) = SizeOf (replace s1 e1 e2)
-- replace s1 e1 (RepBy s2 e2 e3 ) = RepBy s2 (replace s1 e1 e2) (replace s1 e1 e2) --Might not be correct
-- replace s1 e1 (Cond e2 e3 e4) = Cond (replace s1 e1 e2) (replace s1 e1 e3) (replace s1 e1 e4)
-- replace s1 e1 (NewStore e2) = NewStore (replace s1 e1 e2)
-- replace s1 e1 (Dereference s2) = undefined --Is optional
