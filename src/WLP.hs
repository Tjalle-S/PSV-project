{-# LANGUAGE FlexibleContexts #-}

module WLP (makeWLPs, calcWLP, prunedCalcWLP) where

import Expr ( Expr (..), ExprF(..), BinOp(..), Type, prettyishPrintExpr )
import Statement ( ExecStmt(..), ExecTree(..), ExecTreeF(..) )

import Data.Functor.Foldable ( Recursive (cata), Corecursive (embed) )

import Util ( optionalError, MonadG, incrNumPaths, whenRs, ReaderData (..), incrNumPruned )
import TreeBuilder (replace)
import Z3.Monad (MonadZ3, mkNot, mkAnd, assert, getModel, showModel, local, checkAssumptions, getUnsatCore, Result (Sat, Unsat, Undef))
import Z3Util (expr2ast)
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

prunedCalcWLP :: (MonadZ3 m, MonadG m) => Int -> ExecTree -> m Bool
prunedCalcWLP prune tree = cata f tree [] id 0
  where
    f (NodeF (EAssume e) r) as em d = do
      let e' = em e
      whenRs (dumpConditions . options) $
        tell (singleton $ "assumption: " ++ prettyishPrintExpr e')
      ast <- expr2ast (em e)
      let next = map (\g -> g (ast : as) em (d+1)) r
      if prune <= d then do testChildren next else do
        _res <- checkAssumptions (ast : as)
        -- core <- getUnsatCore
        -- tell (singleton $ show core)
        case _res of
          -- Path is feasible, check further
          Sat -> do testChildren next
          -- Path is infeasible, do not check further
          Unsat -> do
            incrNumPruned
            return True
          Undef -> undefined
      
      -- foldrA (&&) True next
    f (NodeF s r) as em d = let next = map (\g -> g as (em . wlpStmt s) (d+1)) r
                          in testChildren next
                          -- in foldrA (&&) True next
    f (TerminationF s) as em _ = do
      incrNumPaths
      let e' = em (wlpStmt s $ LitB True)
      whenRs (dumpConditions . options) $
        tell (singleton $ "goal: " ++ prettyishPrintExpr e')
      local $ do
        ast <- mkNot =<< expr2ast e'
        assert =<< mkAnd (ast : as)
        (_res, model) <- getModel
        case model of
          -- No counterexample found, this WLP is valid.
          Nothing -> return True
          -- Counterexample found, reject this WLP.
          Just m -> do
            ex <- showModel m
            tell (singleton $ unlines ["Reject\n", "Variable assignments:", ex])
            return False

calcWLP :: (MonadZ3 m, MonadG m) => ExecTree -> m Bool
calcWLP tree = cata f tree [] id
  where
    f (NodeF (EAssume e) r)      as em = do
      let e' = em e
      whenRs (dumpConditions . options) $
        tell (singleton $ "assumption: " ++ prettyishPrintExpr e')
      ast <- expr2ast (em e)
      -- Add an extra assumption to the list.
      let next = map (\g -> g (ast : as) em) r
      testChildren next
    -- Not an assumption, add the predicate transformer for this statement.
    f (NodeF s r) as em = let next = map (\g -> g as $ em . wlpStmt s) r
                          in testChildren next
    -- End of the branch, construct negation of assertion, check for counterexample.
    f (TerminationF s) as em = do
      incrNumPaths
      let e' = em (wlpStmt s $ LitB True)
      whenRs (dumpConditions . options) $
        tell (singleton $ "goal: " ++ prettyishPrintExpr e')
      local $ do
        ast <- mkNot =<< expr2ast e'
        assert =<< mkAnd (ast : as)
        (_res, model) <- getModel
        case model of
          -- No counterexample found, this WLP is valid.
          Nothing -> return True
          -- Counterexample found, reject this WLP.
          Just m -> do
            ex <- showModel m
            tell (singleton $ unlines ["Reject\n", "Variable assignments:", ex])
            return False

testChildren :: Monad m => [m Bool] -> m Bool
testChildren []         = return True
testChildren (mb : mbs) = bool (return False) (testChildren mbs) =<< mb

wlpStmt :: ExecStmt -> Expr -> Expr
wlpStmt ESkip             = id
-- wlpStmt (EAssert (LitB True)) = id -- Potential optimisation.
wlpStmt (EAssert e1)      = BinopExpr And e1
wlpStmt (EAssume e1)      = BinopExpr Implication e1
wlpStmt (EAssign s e)     = replace s e
wlpStmt (EAAssign s i e)  = cata f
  where
    f :: ExprF Expr -> Expr
    f (VarF s' t) = replaceVar s' (RepBy (Var s t) i e) s t--foldExpr (defaultAlgebra {var=replaceVar s e})
    f e' = embed e'
wlpStmt (EDrefAssign _ _) = optionalError -- Reference types.
-- wlpStmt EBlock = error "TODO"

replaceVar :: [Char] -> Expr -> [Char] -> Type -> Expr
replaceVar s1 e s2 t| s1==s2 = e
                    | otherwise = Var s1 t

