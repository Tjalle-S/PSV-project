{-# LANGUAGE RecordWildCards #-}

module Algebra where
import GCLParser.GCLDatatype


data ExprAlgebra e
  = ExprAlgebra
    { var :: String -> e

    , litI :: Int -> e
    , litB :: Bool -> e
    , litNull :: e
    , parens :: e->e
    , arrayElem :: e->e->e
    , opNeg :: e->e
    , binopExpr :: BinOp -> e -> e -> e
    , forAll :: String -> e -> e
    , exists :: String -> e -> e
    , sizeOf :: e -> e
    , repBy :: e -> e -> e -> e
    , cond :: e -> e -> e -> e
    , newStore :: e -> e
    , dereference :: String -> e
    }
foldExpr :: ExprAlgebra e -> Expr -> e
foldExpr ExprAlgebra{..} = f
  where
    f (Var s) = var s
    f (LitI i) = litI i
    f (LitB b) = litB b
    f (LitNull)= litNull
    f (Parens e) = parens (f e)
    f (ArrayElem e1 e2) = arrayElem (f e1) (f e2)
    f (OpNeg e) = opNeg (f e)
    f (BinopExpr o e1 e2) = binopExpr o (f e1) (f e2)
    f (Forall s e) = forAll s (f e)
    f (Exists s e) = exists s (f e)
    f (SizeOf e) = sizeOf (f e)
    f (RepBy e1 e2 e3) = repBy (f e1) (f e2) (f e3)
    f (Cond e1 e2 e3) = cond (f e1) (f e2) (f e3)
    f (NewStore e) = newStore (f e)
    f (Dereference s) = dereference s

defaultAlgebra = ExprAlgebra 
  Var 
  LitI 
  LitB 
  LitNull 
  Parens
  ArrayElem
  OpNeg
  BinopExpr
  Forall
  Exists
  SizeOf
  RepBy
  Cond
  NewStore
  Dereference
-- data Expr 
--     = Var                String  
--     | LitI               Int     
--     | LitB               Bool    
--     | LitNull
--     | Parens             Expr    
--     | ArrayElem          Expr   Expr   
--     | OpNeg              Expr    
--     | BinopExpr          BinOp  Expr   Expr
--     | Forall             String Expr 
--     | Exists             String Expr 
--     | SizeOf             Expr
--     | RepBy              Expr   Expr   Expr
--     | Cond               Expr   Expr   Expr
--     | NewStore           Expr
--     | Dereference        String
--     deriving (Eq) 