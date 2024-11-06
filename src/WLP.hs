{-# LANGUAGE FlexibleContexts #-}

module WLP (makeWLPs, calcWLP, prunedCalcWLP) where

import Expr ( Expr (..), ExprF(..), BinOp(..), Type )
import Statement ( ExecStmt(..), ExecTree(..), ExecTreeF(..) )

import Data.Functor.Foldable ( Recursive (cata), Corecursive (embed) )

import Util ( optionalError, V, MonadG, incrNumPaths, incrNumInfeasible)
import TreeBuilder (replace)
import Z3.Monad (MonadZ3, AST, mkNot, mkAnd, assert, getModel, showModel, checkAssumptions, getUnsatCore, Result (Sat, Unsat, Undef), local)
import Z3Util (expr2ast, getValidityCounterExample)
import Control.Monad.RWS (tell)
import Data.DList (singleton)
import Data.Bool (bool)

-- | Create a list of all WLP's of a program, one for each path (lazily).
makeWLPs :: Expr -> ExecTree -> [Expr]
makeWLPs q = cata f
  where
    f (NodeF s ts)     = map (wlpStmt s) (concat ts)
    f (TerminationF s) = [wlpStmt s q]

prunedCalcWLP :: (MonadZ3 m, MonadG m) => Int -> ExecTree -> m Bool
prunedCalcWLP prune tree = cata f tree [] id
  where
    f (NodeF (EAssume e) r) as em = do
      -- tell (singleton "expression")
      ast <- expr2ast (em e)
      -- tell (singleton $ show $ em e)
      let next = map (\g -> g (ast : as) em) r
      if prune <= length as then do testChildren next else do
        _res <- checkAssumptions (ast : as)
        -- core <- getUnsatCore
        -- tell (singleton $ show _res)
        case _res of
          -- Path is feasible, check further
          Sat -> do testChildren next
          -- Path is infeasible, do not check further
          Unsat -> do
            incrNumInfeasible
            -- tell (singleton $ show (length as))
            -- tell (singleton $ show $ em e)
            return True
          Undef -> undefined
      
      -- foldrA (&&) True next
    f (NodeF s r) as em = let next = map (\g -> g as $ em . wlpStmt s) r
                          in testChildren next
                          -- in foldrA (&&) True next
    f (TerminationF s) as em = do
      incrNumPaths     
      -- tell (singleton $ show (em (wlpStmt s $ LitB True)))                                                                                                    
      local $ do
        ast <- mkNot =<< expr2ast (em (wlpStmt s $ LitB True))
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

calcWLP :: (MonadZ3 m, MonadG m) => ExecTree -> m Bool
calcWLP tree = cata f tree [] id
  where
    f (NodeF (EAssume e) r)      as em = do
      ast <- expr2ast (em e)
      let next = map (\g -> g (ast : as) em) r
      testChildren next
      -- foldrA (&&) True next
    f (NodeF s r) as em = let next = map (\g -> g as $ em . wlpStmt s) r
                          in testChildren next
                          -- in foldrA (&&) True next
    f (TerminationF s) as em = do
      incrNumPaths                                                                                                         
      ast <- mkNot =<< expr2ast (em (wlpStmt s $ LitB True))
      assert =<< mkAnd (ast : as)
      (_res, model) <- getModel
      core <- getUnsatCore
      tell (singleton $ show _res)
      tell (singleton $ show core)
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

