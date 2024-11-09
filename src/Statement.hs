{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}


module Statement (ExecStmt(..), ExecTree(..), ExecTreeF(..)) where 

import GCLParser.GCLDatatype (Type(..))

import Expr (Expr)

import Data.Functor.Foldable.TH (makeBaseFunctor)

data ExecStmt = ESkip
              | EAssert Expr
              | EAssume Expr
              | EAssign     String           Expr
              | EAAssign    String           Expr   Expr
              | EDrefAssign String           Expr
              -- | EBlock
              deriving (Show)

type Decl  = (String, Type)
type Decls = [Decl]
data ExecTree = Node ExecStmt [ExecTree]
              | Termination ExecStmt
              | LoopInv ExecTree ExecTree Decls Decls 
              deriving (Show)

makeBaseFunctor ''ExecTree
