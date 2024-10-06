{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}

module Statement (ExecStmt(..), ExecStmtF(..)) where 

import Expr (Expr)

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