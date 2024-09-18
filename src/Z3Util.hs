{-# LANGUAGE FlexibleContexts #-}
module Z3Util (varDecl) where

import GCLParser.GCLDatatype
    ( VarDeclaration(..), Type(..), PrimitiveType(..), Expr (..), BinOp (..) )
import Z3.Monad
import Control.Monad (join)
import Z3Instance ()
import Util (V)

varDecl :: VarDeclaration -> V AST
varDecl (VarDeclaration name typ) = makeVar typ =<< mkStringSymbol name
  where
    makeVar (PType PTInt ) = mkIntVar
    makeVar (PType PTBool) = mkBoolVar
    makeVar RefType        = error "Not implemented"
    makeVar (AType PTInt)  = error "Help!" --mkConstArray $ mkIntSort
    makeVar (AType PTBool) = error "Help!"

expr2ast :: Expr -> Z3 AST
expr2ast (Var name) = undefined
expr2ast (BinopExpr op l r) = join $ mkOp op <$> expr2ast l <*> expr2ast r
  -- zl <- expr2ast l
  -- zr <- expr2ast r
  -- mkOp op zl zr
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