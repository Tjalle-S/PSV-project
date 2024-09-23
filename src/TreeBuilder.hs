{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TemplateHaskell #-}

module TreeBuilder  where
import GCLParser.GCLDatatype hiding (Expr(..))
import qualified GCLParser.GCLDatatype as P
import Data.Maybe (catMaybes,isNothing)
import Data.Fix (Fix (..))
import Data.Functor.Classes (Show1, Eq1)
-- import Generic.Data (Generic(..), Generically(..), Generically1)
import GHC.Generics (Generically1(..), Generic(..))
-- import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Functor.Foldable (Recursive (cata))

data ExprF a
    = Var                String Type
    | LitI               Int     
    | LitB               Bool    
    -- | LitNull
    | ArrayElem          a a   
    | OpNeg              a    
    | BinopExpr          BinOp a a
    | Forall             String a Type
    | Exists             String a Type
    | SizeOf             a
    | RepBy              a a a
    | Cond               a a a
    -- | NewStore           Expr
    -- | Dereference        String
    deriving (Functor, Foldable, Traversable, Generic)

testFoldF :: ExprF Int -> Int
testFoldF (LitI i) = i
testFoldF _ = undefined

testFold :: Expr -> Int
testFold = cata testFoldF

foldExpr :: (ExprF a -> a) -> Expr -> a
foldExpr = cata

deriving via Generically1 ExprF instance Eq1   ExprF
deriving via Generically1 ExprF instance Show1 ExprF

type Expr = Fix ExprF

data ExecTree = Node ExecStmt [ExecTree] | Termination ExecStmt
  deriving (Show)

badExpr2goodExpr :: P.Expr -> Expr
badExpr2goodExpr (P.LitB b)   = Fix (LitB b)
badExpr2goodExpr (P.Parens e) = badExpr2goodExpr e
badExpr2goodExpr (P.BinopExpr op l r) = Fix (BinopExpr op (badExpr2goodExpr l) (badExpr2goodExpr r))
badExpr2goodExpr _ = undefined

-- newtype Fix1 f = Fix { unFix :: f (Fix1 f) }


data ExecStmt = ESkip
              | EAssert Expr
              | EAssume Expr
              | EAssign     String           Expr
              | EAAssign    String           Expr   Expr
              | EDrefAssign String           Expr
              | EBlock      
  deriving (Show)

progToExecMaxDepth :: Int -> Program -> ExecTree
progToExecMaxDepth d  = cut d . progToExec

cut :: Int -> ExecTree -> ExecTree
cut d t = case cut' d t of
            Nothing -> error "All execution paths are too long"
            Just t -> t

cut' :: Int -> ExecTree -> Maybe ExecTree
cut' d t@(Termination _) | d<0 = Nothing
                         | otherwise = Just t
cut' d (Node s ts) | d<0 || length ts'==0= Nothing
                   | otherwise           = Just (Node s ts')
  where
    ts' = catMaybes $ map (cut' (d-1)) ts

progToExec :: Program -> ExecTree
progToExec Program {stmt=s} = stmtToExec s

treeConcat :: ExecTree -> ExecTree -> ExecTree
treeConcat (Node e ts) t2=Node e (map (\t -> treeConcat t t2) ts)
treeConcat (Termination e) t2= Node e [t2]

stmtToExec :: Stmt -> ExecTree
stmtToExec Skip = Termination ESkip
stmtToExec (Assert e)= Termination (EAssert $ badExpr2goodExpr e)
stmtToExec (Assume e)= Termination (EAssume e)
stmtToExec (Assign s e)= Termination (EAssign s e)
stmtToExec (AAssign s i e)= Termination (EAAssign s i e)
stmtToExec (DrefAssign s e)= Termination (EDrefAssign s e)
stmtToExec (Seq s1 s2) = treeConcat (stmtToExec s1) (stmtToExec s2)
stmtToExec (IfThenElse e s1 s2) = Node ESkip [Node (EAssume e) [stmtToExec s1], Node (EAssume (OpNeg e)) [stmtToExec s2]]
stmtToExec s@(While e s') = Node ESkip [Node (EAssume e) [treeConcat (stmtToExec s') (stmtToExec s),Termination (EAssume (OpNeg e))]]
stmtToExec (Block _ s) = stmtToExec s
stmtToExec (TryCatch _ _ _) = undefined

replicateConcat :: Int -> [a] -> [a]
replicateConcat i xs = (concat $ replicate i xs)