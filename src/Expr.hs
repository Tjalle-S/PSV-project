{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DeriveGeneric #-}

module Expr (ExprF(..), Expr(..), prettyishPrintExpr) where

import GCLParser.GCLDatatype (Type(..), BinOp(..))
-- import Data.Fix (Fix)
-- import Generic.Data (Generic1, Generically1(..))
-- import Data.Functor.Classes (Show1)
import Data.Functor.Foldable (Recursive(cata))
import Data.Functor.Foldable.TH (MakeBaseFunctor(makeBaseFunctor))

data Expr
    = Var                String Type
    | LitI               Int     
    | LitB               Bool    
    -- | LitNull
    | ArrayElem          Expr Expr   
    | OpNeg              Expr    
    | BinopExpr          BinOp Expr Expr
    | Forall             String Expr
    | Exists             String Expr
    | SizeOf             String
    | RepBy              Expr Expr Expr
    | Cond               Expr Expr Expr
    deriving (Show, Eq)
    -- | NewStore           Expr
    -- | Dereference        String

makeBaseFunctor ''Expr

-- data ExprF a
--     = Var                String Type
--     | LitI               Int     
--     | LitB               Bool    
--     -- | LitNull
--     | ArrayElem          a a   
--     | OpNeg              a    
--     | BinopExpr          BinOp a a
--     | Forall             String a
--     | Exists             String a
--     | SizeOf             String
--     | RepBy              a a a
--     | Cond               a a a
--     -- | NewStore           Expr
--     -- | Dereference        String
--     deriving (Functor, Generic1)
--     deriving Show1 via Generically1 ExprF

-- type Expr = Fix ExprF

prettyishPrintExpr :: Expr -> String
prettyishPrintExpr = cata prettyishPrintExpr'

prettyishPrintExpr' :: ExprF String -> String
prettyishPrintExpr' (VarF n _) = n
prettyishPrintExpr' (LitIF i)  = show i
prettyishPrintExpr' (LitBF b)  = show b
prettyishPrintExpr' (ArrayElemF a i) = a ++ surroundWith '[' ']' i
prettyishPrintExpr' (OpNegF x) = parens ('~' : x)
prettyishPrintExpr' (BinopExprF op l r) = parens (l ++ surroundWith ' ' ' ' (show op) ++ r)
prettyishPrintExpr' (ForallF n e) = printQuantifier "forall" n e
prettyishPrintExpr' (ExistsF n e) = printQuantifier "exists" n e
prettyishPrintExpr' (SizeOfF e) = '#' : e
prettyishPrintExpr' (RepByF a i e) = a ++ parens (i ++ " repby " ++ e)
prettyishPrintExpr' (CondF c e1 e2) = parens ("if " ++ c ++ " then " ++ e1 ++ " else " ++ e2)

printQuantifier :: String -> String -> String -> String
printQuantifier q n e = parens (q ++ n ++ ": " ++ e)

surroundWith :: Char -> Char -> String -> String
surroundWith l r x = l : x ++ [r]

parens :: String -> String
parens = surroundWith '(' ')'