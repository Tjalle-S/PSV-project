{-# LANGUAGE FlexibleContexts #-}

module WLP (prunedCalcWLP) where

import Expr ( Expr (..), ExprF(..), BinOp(..), Type )
import Statement ( ExecStmt(..), ExecTree(..), ExecTreeF(..) )

import Data.Functor.Foldable ( Recursive (cata), Corecursive (embed) )

import Util ( optionalError, MonadG, incrNumPaths, incrNumPruned, whenRs)
import TreeBuilder (replace)
import Z3.Monad (MonadZ3, mkNot, mkAnd, assert, getModel, showModel, checkAssumptions, Result (..), local)
import Z3Util (expr2ast)
import Control.Monad.RWS (tell)
import Data.DList (singleton)
import Data.Bool (bool)
import Cli (ArgData(dumpConditions))

prunedCalcWLP :: (MonadZ3 m, MonadG m) => Int -> ExecTree -> m Bool
prunedCalcWLP prune tree = cata f tree [] id 0
  where
    f (NodeF (EAssume e) r) as em d = do
      let e' = em e
      ast <- expr2ast e'
      let next = map (\g -> g (ast : as) em (d + 1)) r
      whenRs dumpConditions $ do
        tell (singleton $ show e')
      if prune <= d then do testChildren next else do
        _res <- checkAssumptions (ast : as)
        case _res of
          -- Path is feasible, check further
          Sat   -> testChildren next
          -- Path is infeasible, do not check further
          Unsat -> incrNumPruned >> return True
          _     -> error "Got undefined result from Z3"
      
    f (NodeF s r) as em d = let next = map (\g -> g as (em . wlpStmt s) (d + 1)) r
                            in testChildren next
    f (TerminationF s) as em _ = do
      incrNumPaths
      local $ do
        let e' = em (wlpStmt s $ LitB True)                                                                                      
        ast <- mkNot =<< expr2ast e'
        whenRs dumpConditions $ do
          tell (singleton $ show e')
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
    f (LoopInvF {}) _ _ _ = error "Unreachable: loop invariants removed after cut."

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
-- wlpStmt EBlock = error "TODO"

replaceVar :: [Char] -> Expr -> [Char] -> Type -> Expr
replaceVar s1 e s2 t| s1==s2 = e
                    | otherwise = Var s1 t

