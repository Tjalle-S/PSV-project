{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}

module Z3Util (expr2ast, isSatisfiable, isValid, Expr, ExprF(..)) where

import GCLParser.GCLDatatype (Type(..), BinOp(..), PrimitiveType (..))
import Z3.Monad
import Control.Monad (join)
import Util (VState (..), Stats (..))
import Generic.Data (Generic1, Generically1(..))
import Data.Functor.Classes (Show1)
import Data.Fix (Fix)
import Data.Functor.Foldable (Recursive(cata))

import Z3Instance ()
import Control.Monad.State (MonadState, modify')

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

-- ============================================================

data ExprF a
    = Var                String Type

    | LitI               Int
    | LitB               Bool

    | ArrayElem          a a
    | RepBy              a a a
    | SizeOf             String

    | OpNeg              a
    | BinopExpr          BinOp a a

    | Forall             String a Type
    | Exists             String a Type

    | Cond               a a a
    deriving (Functor, Generic1)
    deriving Show1 via Generically1 ExprF

type Expr = Fix ExprF

expr2astF :: (MonadZ3 m, MonadState VState m) => ExprF (m AST) -> m AST
expr2astF (Var name typ)       = incrSize >> makeVar name typ

expr2astF (LitI i)             = incrSize >> mkIntNum i
expr2astF (LitB b)             = incrSize >> mkBool b

expr2astF (OpNeg me)           = do
  e    <- me
  sort <- getSortKind =<< getSort e
  case sort of
    Z3_BOOL_SORT -> mkNot        e
    Z3_INT_SORT  -> mkUnaryMinus e
    other        -> error ("Panic! Cannot negate type " ++ show other) -- Should never occur.
expr2astF (BinopExpr op e1 e2) = join (mkOp op <$> e1 <*> e2)

expr2astF (Cond c t f)         = join (mkIte <$> c <*> t <*> f)

expr2astF (ArrayElem a i  )    = join (mkSelect <$> a <*> i)
expr2astF (RepBy     a i e)    = join (mkStore  <$> a <*> i <*> e)
expr2astF (SizeOf    a    )    = mkIntVar =<< mkStringSymbol ('#' : a) -- Why does the datatype have the array as an expression instead of a string?

expr2astF (Forall name e typ)  = mkQuantifier mkForall name typ e
expr2astF (Exists name e typ)  = mkQuantifier mkExists name typ e

makeVar :: MonadZ3 m => String -> Type -> m AST
makeVar name typ = join (mkVar <$> mkStringSymbol name <*> makeSort typ)

makeSort :: MonadZ3 m => Type -> m Sort
makeSort (PType t) = makePrimSort t
makeSort (AType t) = join (mkArraySort <$> mkIntSort <*> makePrimSort t)
makeSort RefType   = error "Not implemented" -- Pointer types.

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

mkOp Alias            = error "Not implemented" -- Reference equality.

uncurryList :: ([a] -> b) -> a -> a -> b
uncurryList op l r = op [l, r]

mkQuantifier :: MonadZ3 m => ([Pattern] -> [Symbol] -> [Sort] -> AST -> m AST) -> String -> Type -> m AST -> m AST
mkQuantifier q name typ e = do
  symb <- mkStringSymbol name
  sort <- makeSort typ
  q [] [symb] [sort] =<< e

incrSize :: (MonadState VState m) => m ()
incrSize = modify' $ \v@VState { stats = s@Stats { formulaSize } } -> v { stats = s { formulaSize = formulaSize + 1 } }