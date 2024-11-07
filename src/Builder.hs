{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module Builder where

import GCLParser.GCLDatatype
import Data.Functor.Foldable.TH (MakeBaseFunctor(makeBaseFunctor))
import Data.Functor.Foldable (cata)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (local))
import Z3.Monad hiding (local)
import qualified Z3.Monad as Z3
import Control.Monad (join)
import Util (optionalError)
import Data.List (unionBy)
import Data.Function (on)
-- import Z3Instance ()

-- data ExecutionTree = ExContinue Stmt ExecutionTree
--                    | ExConditional Expr ExecutionTree ExecutionTree
--                    | ExBlock [VarDeclaration] ExecutionTree
--                    | ExTip Stmt

makeBaseFunctor [''Stmt, ''Expr]

-- buildProgramTree :: Program -> ExecutionTree
-- buildProgramTree Program { input, output, stmt } = ExBlock (input ++ output) (buildTree stmt)

-- buildTree :: Stmt -> ExecutionTree
-- buildTree = cata buildTree'

-- buildTree' :: StmtF ExecutionTree -> ExecutionTree
-- buildTree' (BlockF decls tree) = ExBlock decls tree
-- buildTree' (IfThenElse branchT branchE) = ExConditional 
-- buildTree' Skip = ExTip Skip
-- buildTree' (Assert e) = ExTip ()

type Decl = (String, AST)
type Decls = [Decl]

data Bindings = Bindings {
  vars   :: Decls
, qbound :: Decls
, qDepth :: Int
}

type Verify = ReaderT Bindings Z3

makeVar :: MonadZ3 m => VarDeclaration -> m Decl
makeVar (VarDeclaration name typ) = do
  ast <- mkFreshVar name =<< makeSort typ
  return (name, ast)

makeVars :: MonadZ3 m => [VarDeclaration] -> m Decls
makeVars = mapM makeVar

makeSort :: MonadZ3 m => Type -> m Sort
makeSort (PType t) = makePrimSort t
makeSort (AType t) = join (mkArraySort <$> mkIntSort <*> makePrimSort t)
makeSort RefType   = optionalError -- Pointer types.

makePrimSort :: MonadZ3 m => PrimitiveType -> m Sort
makePrimSort PTInt  = mkIntSort
makePrimSort PTBool = mkBoolSort

verifyProgram :: Program -> IO Bool
verifyProgram Program { input, output, stmt } = evalZ3 $ runReaderT (verify stmt) $ Bindings { vars = [] }

verify :: Stmt -> Verify Bool
verify = cata verify'

verify' :: StmtF (Verify Bool) -> Verify Bool
verify' (BlockF decls res) = do
  newVars <- makeVars decls
  local (updateVars newVars) res


updateVars :: Decls -> Bindings -> Bindings
updateVars newVars b@Bindings { vars } = b { vars = updateDecls newVars vars }

-- | Updates declarations.
-- @updateDecls new old@ will be the union of old and new, with values from new overriding those from old.
updateDecls :: Decls -> Decls -> Decls
updateDecls = unionBy ((==) `on` fst)

  -- ExSkip
  --                  | ExAssert Expr
  --                  | ExAssume Expr
  --                  | ExAssign Expr
  --                  | 