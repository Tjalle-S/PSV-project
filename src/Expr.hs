{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Expr (
  ExprF(..)
, Expr(..)
, prettyishPrintExpr
-- Re-export necessary types from the GCL parser.
, Type(..)
, PrimitiveType(..)
, BinOp(..)
) where

import GCLParser.GCLDatatype (Type(..), PrimitiveType(..), BinOp(..))

import Data.Functor.Foldable (Recursive(cata))
import Data.Functor.Foldable.TH (MakeBaseFunctor(makeBaseFunctor))

data Expr
    = Var       String Type
    | LitI      Int     
    | LitB      Bool    
    -- LitNull
    | ArrayElem Expr Expr   
    | OpNeg     Expr    
    | BinopExpr BinOp Expr Expr
    | Forall    String Expr
    | Exists    String Expr
    | SizeOf    String
    | RepBy     Expr Expr Expr
    | Cond      Expr Expr Expr
    -- | NewStore           Expr
    -- | Dereference        String
    deriving (Show, Eq)

makeBaseFunctor ''Expr

prettyishPrintExpr :: Expr -> String
prettyishPrintExpr = cata prettyishPrintExpr'

prettyishPrintExpr' :: ExprF String -> String
prettyishPrintExpr' (VarF n _)          = n
prettyishPrintExpr' (LitIF i)           = show i
prettyishPrintExpr' (LitBF b)           = show b
prettyishPrintExpr' (ArrayElemF a i)    = a ++ surroundWith '[' ']' i
prettyishPrintExpr' (OpNegF x)          = parens ('~' : x)
prettyishPrintExpr' (BinopExprF op l r) = parens (l ++ surroundWith ' ' ' ' (show op) ++ r)
prettyishPrintExpr' (ForallF n e)       = printQuantifier "forall" n e
prettyishPrintExpr' (ExistsF n e)       = printQuantifier "exists" n e
prettyishPrintExpr' (SizeOfF e)         = '#' : e
prettyishPrintExpr' (RepByF a i e)      = a ++ parens (i ++ " repby " ++ e)
prettyishPrintExpr' (CondF c e1 e2)     = parens ("if " ++ c ++ " then " ++ e1 ++ " else " ++ e2)

printQuantifier :: String -> String -> String -> String
printQuantifier q n e = parens (q ++ " " ++ n ++ ": " ++ e)

surroundWith :: Char -> Char -> String -> String
surroundWith l r x = l : x ++ [r]

parens :: String -> String
parens = surroundWith '(' ')'
