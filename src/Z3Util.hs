{-# LANGUAGE FlexibleContexts #-}
module Z3Util (varDecl, expr2ast, isSatisfiable, isValid) where

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
    makeVar (AType t     ) = mkArrayVar t
    makeVar RefType        = error "Not implemented"

    mkArrayVar t s = mkVar s =<< join (mkArraySort <$> mkIntSort <*> makePrimSort t)

    makePrimSort PTInt  = mkIntSort
    makePrimSort PTBool = mkBoolSort

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
mkOp And = mkListOp mkAnd
mkOp Or = mkListOp mkOr
mkOp _ = undefined


mkListOp :: ([a] -> t) -> a -> a -> t
mkListOp op l r = op [l, r]

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