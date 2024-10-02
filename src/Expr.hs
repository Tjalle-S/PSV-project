{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Expr where

import GCLParser.GCLDatatype (Type(..), BinOp(..))
import Data.Fix (Fix)
import Generic.Data (Generic1, Generically1(..))
import Data.Functor.Classes (Show1)

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
