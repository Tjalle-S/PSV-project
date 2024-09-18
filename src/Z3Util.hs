{-# LANGUAGE FlexibleContexts #-}
module Z3Util (varDecl) where

import GCLParser.GCLDatatype
    ( VarDeclaration(..), Type(..), PrimitiveType(..), Expr (..), BinOp (..) )
import Z3.Monad
import Util (MonadG, runV)
-- import Control.Monad.RWS (RWST(runRWST), MonadReader)
import Cli (ArgData)
import Control.Monad.Reader (ReaderT, MonadTrans)
import Control.Monad.State (StateT, MonadState (state))

type Symbols = ReaderT () Z3

-- instance MonadZ3 m => MonadZ3 (StateT s m) where
--   getSolver = state
  

-- test1 = runV undefined (varDecl undefined)

varDecl :: VarDeclaration -> Symbols AST
varDecl (VarDeclaration name typ) = makeVar typ =<< mkStringSymbol name
  where
    makeVar (PType PTInt ) = mkIntVar
    makeVar (PType PTBool) = mkBoolVar
    makeVar RefType        = error "Not implemented"
    makeVar (AType PTInt)  = error "Help!" --mkConstArray $ mkIntSort
    makeVar (AType PTBool) = error "Help!"

expr2ast :: Expr -> Z3 AST
expr2ast (Var name) = undefined
expr2ast (BinopExpr op l r) = do
  zl <- expr2ast l
  zr <- expr2ast r
  mkOp op zl zr
expr2ast _ = undefined

mkOp :: BinOp -> AST -> AST -> Z3 AST
mkOp Implication = mkImplies
mkOp LessThan = mkLt
mkOp GreaterThan = mkGt
mkOp LessThanEqual = mkLe
mkOp GreaterThanEqual = mkGe
mkOp _ = undefined

makePair :: a -> a -> [a]
makePair x y = [x, y]

isSatisfiable :: AST -> Z3 Bool
isSatisfiable ast = test . fst <$ assert ast <*> getModel
  where
    test Sat = True
    test _   = False

isValid :: AST -> Z3 Bool
isValid ast = test . fst <$ (assert =<< mkNot ast) <*> getModel
  where
    test Unsat = True
    test _     = False