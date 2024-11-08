{-# LANGUAGE FlexibleContexts #-}

module Z3Util (expr2ast, isSatisfiable, isValid, getValidityCounterExample) where

import GCLParser.GCLDatatype ( Type(..), BinOp(..), PrimitiveType (..) )
import Expr ( Expr, ExprF(..) )

import Z3.Monad
import Z3Instance ()

import Control.Monad ( join )
import Control.Monad.State ( MonadState )
import Data.Functor.Foldable ( Recursive(cata) )

import Util ( VState(..), optionalError, incrFormulaSize )


-- | Transforms an expression into a Z3 AST.
expr2ast :: (MonadZ3 m, MonadState VState m) => Expr -> m AST
expr2ast = cata expr2astF

-- | Checks if the given expression is satisfiable.
isSatisfiable :: MonadZ3 m => AST -> m Bool
isSatisfiable ast = test . fst <$ assert ast <*> getModel
  where
    test Sat = True
    test _   = False

-- | Checks if the given expression is valid.
isValid :: MonadZ3 m => AST -> m Bool
isValid ast = test . fst <$ (assert =<< mkNot ast) <*> getModel
  where
    test Unsat = True
    test _     = False

-- | Attempts to find a counterexample to show the assertion is not valid.
getValidityCounterExample :: MonadZ3 m => [AST] -> AST -> m (Maybe String)
getValidityCounterExample as ast = do
  assert =<< mkNot ast
  res <- checkAssumptions as
  case res of
    Sat -> Just <$> (showModel =<< solverGetModel)
    _   -> return Nothing
    -- Nothing -> return Nothing
    -- Just m  -> Just <$> showModel m

-- ============================================================

expr2astF :: (MonadZ3 m, MonadState VState m) => ExprF (m AST) -> m AST
expr2astF (VarF name (typ,_))   = incrFormulaSize >> makeVar name typ

expr2astF (LitIF i)             = incrFormulaSize >> mkIntNum i
expr2astF (LitBF b)             = incrFormulaSize >> mkBool b

expr2astF (OpNegF e)            = mkNot =<< e

expr2astF (BinopExprF op e1 e2) = join (mkOp op <$> e1 <*> e2)

expr2astF (CondF c t f)         = join (mkIte <$> c <*> t <*> f)

expr2astF (ArrayElemF a i  )    = join (mkSelect <$> a <*> i)
expr2astF (RepByF     a i e)    = join (mkStore  <$> a <*> i <*> e)
expr2astF (SizeOfF    a    )    = incrFormulaSize >> (mkIntVar =<< mkStringSymbol ('#' : a))

expr2astF (ForallF name e)      = mkQuantifier mkForall name e
expr2astF (ExistsF name e)      = mkQuantifier mkExists name e

makeVar :: MonadZ3 m => String -> Type -> m AST
makeVar name typ = join (mkVar <$> mkStringSymbol name <*> makeSort typ)

makeSort :: MonadZ3 m => Type -> m Sort
makeSort (PType t) = makePrimSort t
makeSort (AType t) = join (mkArraySort <$> mkIntSort <*> makePrimSort t)
makeSort RefType   = optionalError -- Pointer types.

makePrimSort :: MonadZ3 m => PrimitiveType -> m Sort
makePrimSort PTInt  = mkIntSort
makePrimSort PTBool = mkBoolSort

mkOp :: MonadZ3 m => BinOp -> AST -> AST -> m AST
mkOp And              = uncurryList mkAnd
mkOp Or               = uncurryList mkOr
mkOp Implication      = mkImplies

mkOp Equal            = mkEq
mkOp LessThan         = mkLt
mkOp LessThanEqual    = mkLe
mkOp GreaterThan      = mkGt
mkOp GreaterThanEqual = mkGe

mkOp Plus             = uncurryList mkAdd
mkOp Minus            = uncurryList mkSub
mkOp Multiply         = uncurryList mkMul
mkOp Divide           = mkDiv

mkOp Alias            = optionalError -- Reference equality.

uncurryList :: ([a] -> b) -> a -> a -> b
uncurryList op l r = op [l, r]

mkQuantifier :: MonadZ3 m => ([Pattern] -> [Symbol] -> [Sort] -> AST -> m AST) -> String -> m AST -> m AST
mkQuantifier q name e = do
  symb <- mkStringSymbol name
  sort <- mkIntSort
  q [] [symb] [sort] =<< e
