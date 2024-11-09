{-# LANGUAGE FlexibleContexts #-}

module WLP (prunedCalcWLP) where

import Expr ( Expr (..), ExprF(..), BinOp(..), Type )
import Statement ( ExecStmt(..), ExecTree(..), ExecTreeF(..) )

import Data.Functor.Foldable ( Recursive (cata), Corecursive (embed) )

import Util ( optionalError, MonadG, incrNumPaths, incrNumPruned, whenRs, applyWhen)
import TreeBuilder (replace, simplifyExpr)
import Z3.Monad (MonadZ3, mkNot, mkAnd, assert, getModel, showModel, checkAssumptions, Result (..), local, AST, Model)
import Z3Util (expr2ast)
import Control.Monad.RWS (tell)
import Data.DList (singleton)
import Data.Bool (bool)
import Cli (ArgData(dumpConditions))

prunedCalcWLP :: (MonadZ3 m, MonadG m) => Bool -> Int -> ExecTree -> m Bool
prunedCalcWLP simp prune tree = cata f tree [] id 0
  where
    f (NodeF (EAssert e) r) as em d = do
      -- Check assertion, then ignore it afterwards.
      (_res, model) <- local $ do
        e' <- getTransformed em "assert: " (applyWhen simp simplifyExpr e)
        ast <- mkNot =<< expr2ast e'
        assert =<< mkAnd (ast : as)
        getModel
      checkAssertions model (continue as em d r)
    f (NodeF (EAssume e) r) as em d = do
      e' <- getTransformed em "assume: " (applyWhen simp simplifyExpr e)
      ast <- expr2ast e'
      -- Add the assumption to the list.
      let continuation = continue (ast : as) em d r
      if prune <= d then do continuation else do
        _res <- checkAssumptions (ast : as)
        case _res of
          -- Path is feasible, check further
          Sat   -> continuation
          -- Path is infeasible, do not check further
          Unsat -> incrNumPruned >> return True
          _     -> error "Got undefined result from Z3"
    -- Add transformer to the composition.
    f (NodeF s r) as em d = continue as (em . applyWhen simp simplifyExpr . wlpStmt s) d r
    -- End of path. Perform last transformation, then check assertions.
    f (TerminationF s) as em _ = do
      incrNumPaths
      local $ do
        e' <- getTransformed em "end: " (wlpStmt s $ LitB True)                                                                                     
        ast <- mkNot =<< expr2ast e'
        assert =<< mkAnd (ast : as)
        (_res, model) <- getModel
        checkAssertions model (return True)
    f (LoopInvF {}) _ _ _ = error "Unreachable: loop invariants removed after cut."

-- | Get the result of the continuation along the path.
continue :: Monad m => [AST] -> (Expr -> Expr) -> Int -> [[AST] -> (Expr -> Expr) -> Int -> m Bool] -> m Bool
continue as em d = testChildren . map (\g -> g as em (d + 1))

-- | Transform an expression, and print it if necessary.
getTransformed :: (MonadG m) => (Expr -> Expr) -> String -> Expr -> m Expr
getTransformed em t e = do 
  let e' = em e 
  whenRs dumpConditions $ tell (singleton $ t ++ show e')
  return e'

checkAssertions :: (MonadG m, MonadZ3 m) => Maybe Model -> m Bool -> m Bool
-- No counterexample found. Continue or accept.
checkAssertions Nothing  continuation = continuation
-- Counterexample found. Print it and stop.
checkAssertions (Just m) _            = do
  ex <- showModel m
  tell (singleton $ unlines ["Reject\n", "Variable assignments:", ex])
  return False

testChildren :: Monad m => [m Bool] -> m Bool
testChildren []         = return True
testChildren (mb : mbs) = bool (return False) (testChildren mbs) =<< mb

wlpStmt :: ExecStmt -> Expr -> Expr
wlpStmt ESkip             = id
wlpStmt (EAssert e1)      = BinopExpr And e1
wlpStmt (EAssume e1)      = BinopExpr Implication e1
wlpStmt (EAssign s e)     = replace s e
wlpStmt (EAAssign s i e)  = cata f
  where
    f :: ExprF Expr -> Expr
    f (VarF s' t) = replaceVar s' (RepBy (Var s t) i e) s t
    f e' = embed e'
wlpStmt (EDrefAssign _ _) = optionalError -- Reference types.

replaceVar :: [Char] -> Expr -> [Char] -> Type -> Expr
replaceVar s1 e s2 t| s1==s2 = e
                    | otherwise = Var s1 t

