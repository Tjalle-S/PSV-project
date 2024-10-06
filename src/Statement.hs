{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}

module Statement (ExecStmt(..), ExecTree(..), ExecTreeF(..)) where 

import Expr (Expr)

import Data.Functor.Foldable.TH (makeBaseFunctor)

data ExecStmt = ESkip
              | EAssert Expr
              | EAssume Expr
              | EAssign     String           Expr
              | EAAssign    String           Expr   Expr
              | EDrefAssign String           Expr
              | EBlock
              deriving (Show)

data ExecTree = Node ExecStmt [ExecTree]
              | Termination ExecStmt
              deriving (Show)

makeBaseFunctor ''ExecTree