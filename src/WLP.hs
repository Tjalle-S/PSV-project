module WLP (wlpTree, makeWLPs) where

import Expr ( Expr (..), ExprF(..) )
import Statement ( ExecStmt(..), ExecTree(..), ExecTreeF(..) )
import GCLParser.GCLDatatype ( BinOp(..), Type )

import Data.Functor.Foldable ( Recursive (cata), Corecursive (embed) )

import Util ( optionalError )
import TreeBuilder (replace)

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
wlpStmt (EAssign s e) = replace s e
wlpStmt (EAAssign s i e) = cata f --replace s (RepBy (Var s t) i e) --foldExpr (defaultAlgebra {var=replaceVar s (RepBy (Var s) i e)})
  where
    f :: ExprF Expr -> Expr
    f (VarF s' t) = replaceVar s' (RepBy (Var s t) i e) s t--foldExpr (defaultAlgebra {var=replaceVar s e})
    f e' = embed e'
wlpStmt (EDrefAssign _ _) = optionalError -- Reference types.
wlpStmt EBlock = error "TODO"

replaceVar :: [Char] -> Expr -> [Char] -> Type -> Expr
replaceVar s1 e s2 t| s1==s2 = e
                    | otherwise = Var s1 t

