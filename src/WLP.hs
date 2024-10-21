{-# LANGUAGE FlexibleContexts #-}

module WLP (makeWLPs, calcWLP) where

import Expr ( Expr (..), ExprF(..), BinOp(..), Type, prettyishPrintExpr )
import Statement ( ExecStmt(..), ExecTree(..), ExecTreeF(..) )

import Data.Functor.Foldable ( Recursive (cata), Corecursive (embed) )

import Util ( optionalError, V, MonadG, incrNumPaths, whenRs, ReaderData (..) )
import TreeBuilder (replace)
import Z3.Monad (MonadZ3, AST, mkNot, mkAnd, assert, getModel, showModel)
import Z3Util (expr2ast, getValidityCounterExample)
import Control.Monad.RWS (tell)
import Data.DList (singleton)
import Data.Bool (bool)
import Cli (ArgData(dumpConditions))

-- | Create a list of all WLP's of a program, one for each path (lazily).
makeWLPs :: Expr -> ExecTree -> [Expr]
makeWLPs q = cata f
  where
    f (NodeF s ts)     = map (wlpStmt s) (concat ts)
    f (TerminationF s) = [wlpStmt s q]

calcWLP :: (MonadZ3 m, MonadG m) => ExecTree -> m Bool
calcWLP tree = cata f tree [] id
  where
    f (NodeF (EAssume e) r)      as em = do
      let e' = em e
      whenRs (dumpConditions . options) $
        tell (singleton $ "assumption: " ++ prettyishPrintExpr e')
      ast <- expr2ast (em e)
      let next = map (\g -> g (ast : as) em) r
      testChildren next
      -- foldrA (&&) True next
    f (NodeF s r) as em = let next = map (\g -> g as $ em . wlpStmt s) r
                          in testChildren next
                          -- in foldrA (&&) True next
    f (TerminationF s) as em = do
      incrNumPaths
      let e' = em (wlpStmt s $ LitB True)
      whenRs (dumpConditions . options) $
        tell (singleton $ "goal: " ++ prettyishPrintExpr e')
      ast <- mkNot =<< expr2ast e'
      assert =<< mkAnd (ast : as)
      (_res, model) <- getModel
      case model of
        -- No counterexample found, check next WLP.
        Nothing -> return True
        -- Counterexample found, stop here.
        Just m -> do
          ex <- showModel m
          tell (singleton $ unlines ["Reject\n", "Variable assignments:", ex])
          return False

-- foldrA :: (Foldable t, Applicative m) => (a -> b -> b) -> b -> t (m a) -> m b
-- foldrA f e = foldr (\c r -> f <$> c <*> r) (pure e)

testChildren :: Monad m => [m Bool] -> m Bool
testChildren []         = return True
testChildren (mb : mbs) = bool (return False) (testChildren mbs) =<< mb
  -- b <- mb
  -- if b
  --   then testChildren mbs
  --   else return False

-- wlpTree :: ExecTree -> Expr -> Expr
-- wlpTree (Node s ts) q = wlpStmt s wlpChildrenCombined
--   where
--     wlpChildren = map (`wlpTree` q) ts
--     wlpChildrenCombined = foldr1 (BinopExpr And) wlpChildren
-- wlpTree (Termination s) q = wlpStmt s q



wlpStmt :: ExecStmt -> Expr -> Expr
wlpStmt ESkip             = id
-- wlpStmt (EAssert (LitB True)) = id -- Potential optimisation.
wlpStmt (EAssert e1)      = BinopExpr And e1
wlpStmt (EAssume e1)      = BinopExpr Implication e1
wlpStmt (EAssign s e)     = replace s e
wlpStmt (EAAssign s i e)  = cata f --replace s (RepBy (Var s t) i e) --foldExpr (defaultAlgebra {var=replaceVar s (RepBy (Var s) i e)})
  where
    f :: ExprF Expr -> Expr
    f (VarF s' t) = replaceVar s' (RepBy (Var s t) i e) s t--foldExpr (defaultAlgebra {var=replaceVar s e})
    f e' = embed e'
wlpStmt (EDrefAssign _ _) = optionalError -- Reference types.
-- wlpStmt EBlock = error "TODO"

replaceVar :: [Char] -> Expr -> [Char] -> Type -> Expr
replaceVar s1 e s2 t| s1==s2 = e
                    | otherwise = Var s1 t

