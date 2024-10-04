{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Expr (ExprF(..), Expr, prettyishPrintExpr) where

import GCLParser.GCLDatatype (Type(..), BinOp(..))
import Data.Fix (Fix)
import Generic.Data (Generic1, Generically1(..))
import Data.Functor.Classes (Show1)
import Data.Functor.Foldable (Recursive(cata))

data ExprF a
    = Var                String Type
    | LitI               Int     
    | LitB               Bool    
    -- | LitNull
    | ArrayElem          a a   
    | OpNeg              a    
    | BinopExpr          BinOp a a
    | Forall             String a
    | Exists             String a
    | SizeOf             String
    | RepBy              a a a
    | Cond               a a a
    -- | NewStore           Expr
    -- | Dereference        String
    deriving (Functor, Generic1)
    deriving Show1 via Generically1 ExprF

type Expr = Fix ExprF

prettyishPrintExpr :: Expr -> String
prettyishPrintExpr = cata prettyishPrintExpr'

prettyishPrintExpr' :: ExprF String -> String
prettyishPrintExpr' (Var n _) = n
prettyishPrintExpr' (LitI i)  = show i
prettyishPrintExpr' (LitB b)  = show b
prettyishPrintExpr' (ArrayElem a i) = a ++ surroundWith '[' ']' i
prettyishPrintExpr' (OpNeg x) = parens ('~' : x)
prettyishPrintExpr' (BinopExpr op l r) = parens (l ++ surroundWith ' ' ' ' (show op) ++ r)
prettyishPrintExpr' (Forall n e) = printQuantifier "forall" n e
prettyishPrintExpr' (Exists n e) = printQuantifier "exists" n e
prettyishPrintExpr' (SizeOf e) = '#' : e
prettyishPrintExpr' (RepBy a i e) = a ++ parens (i ++ " repby " ++ e)
prettyishPrintExpr' (Cond c e1 e2) = parens ("if " ++ c ++ " then " ++ e1 ++ " else " ++ e2)

printQuantifier :: String -> String -> String -> String
printQuantifier q n e = parens (q ++ n ++ ": " ++ e)

surroundWith :: Char -> Char -> String -> String
surroundWith l r x = l : x ++ [r]

parens :: String -> String
parens = surroundWith '(' ')'