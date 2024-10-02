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
import Generic.Data (Generic(..), Generically(..), Generically1(..), Generic1(..))
import Control.Monad.State.Lazy
-- import GHC.Generics (Generically1(..), Generic(..))
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
    | SizeOf             String
    | RepBy              a a a
    | Cond               a a a
    -- | NewStore           Expr
    -- | Dereference        String
    deriving (Functor, Foldable, Traversable, Generic, Generic1)




testFoldF :: ExprF Int -> Int
testFoldF (LitI i) = i
testFoldF _ = undefined

testFold :: Expr -> Int
testFold = cata testFoldF

foldExpr :: (ExprF a -> a) -> Expr -> a
foldExpr = cata

-- deriving via Generically1 ExprF instance Eq1   ExprF
deriving via Generically1 ExprF instance Show1 ExprF

type Expr = Fix ExprF

data ExecTree = Node ExecStmt [ExecTree] | Termination ExecStmt
  deriving (Show)

makeUnique :: String -> State [(String,Type)] String
makeUnique s= do
                    vars <- get
                    let names =map fst vars  in
                      if s `elem` names  
                        then makeUnique (s++"\'")
                      else
                        return s

findType :: String -> State [(String,Type)] Type
findType s = do
                vars <- get
                case lookup s vars of
                  Just t -> return t
                  Nothing -> error $ "Variable "++s++" does not exist."
addVar :: (String,Type) -> State [(String,Type)] String
addVar (s,t) = do
                  s' <- makeUnique s
                  vars <- get
                  put ((s',t):vars)
                  return s'
--copied from WLP
replace :: String -> Expr -> Expr -> Expr
replace s e1 e2 = cata f e2
  where
    f :: ExprF Expr -> Expr
    f (Var s' t) =  replaceVar s' e1 s--foldExpr (defaultAlgebra {var=replaceVar s e})
    f e' = Fix e'

replaceVar :: [Char] -> Expr -> [Char] -> Expr
replaceVar s1 e s2 | s1==s2 = e
                   | otherwise = Fix $ Var s2 (PType PTInt) --This is temporary

getVarStr :: P.Expr -> String
getVarStr (P.Var s) = s
getVarStr (P.RepBy e _ _) = getVarStr e
getVarStr e = error $ show e ++ " is not an array"

badExpr2goodExpr :: P.Expr -> State [(String,Type)] Expr
badExpr2goodExpr (P.Var s)            = do
                                          t<-findType s
                                          return $ Fix (Var s t)
badExpr2goodExpr (P.LitI i)           = return $Fix (LitI i)
badExpr2goodExpr (P.LitB b)           = return $Fix (LitB b)
badExpr2goodExpr (P.LitNull)          = return $undefined
badExpr2goodExpr (P.Parens e)         = badExpr2goodExpr e
badExpr2goodExpr (P.ArrayElem e1 e2)  = do 
                                          e1' <- badExpr2goodExpr e1
                                          e2' <- badExpr2goodExpr e2
                                          return $Fix $ ArrayElem e1' e2'
badExpr2goodExpr (P.OpNeg e)          = do
                                          e' <- badExpr2goodExpr e
                                          return $Fix $ OpNeg e'
badExpr2goodExpr (P.BinopExpr op l r) = do
                                          l' <- badExpr2goodExpr l
                                          r' <- badExpr2goodExpr r
                                          return $Fix (BinopExpr op l' r')
badExpr2goodExpr (P.Forall s e)       = do
                                          s' <- addVar (s,PType PTInt)--For now I will assume that it is always an integer
                                          e' <- badExpr2goodExpr e
                                          return $Fix (Forall s' (replace s (Fix $ Var s' (PType PTInt)) e') (PType PTInt))
badExpr2goodExpr (P.Exists s e)       = do
                                          e' <- badExpr2goodExpr e
                                          s' <- addVar (s,PType PTInt)--For now I will assume that it is always an integer
                                          return $Fix (Exists s' (replace s (Fix $ Var s' (PType PTInt)) e') (PType PTInt))
badExpr2goodExpr (P.SizeOf e)         = do
                                          return $Fix (SizeOf $ getVarStr e)
badExpr2goodExpr (P.RepBy e1 e2 e3)   = do
                                          e1' <- badExpr2goodExpr e1
                                          e2' <- badExpr2goodExpr e2
                                          e3' <- badExpr2goodExpr e3
                                          return $Fix (RepBy e1' e2' e3')
badExpr2goodExpr (P.Cond e1 e2 e3)    = do
                                          e1' <- badExpr2goodExpr e1
                                          e2' <- badExpr2goodExpr e2
                                          e3' <- badExpr2goodExpr e3
                                          return $Fix $ Cond e1' e2' e3'
badExpr2goodExpr (P.NewStore e)       = return $undefined--Fix $ NewStore (badExpr2goodExpr e)
badExpr2goodExpr (P.Dereference s)    = return $undefined

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

varDeclsToTuples :: [VarDeclaration]-> [(String,Type)]
varDeclsToTuples = map (\(VarDeclaration s t)->(s,t))

progToExec :: Program -> ExecTree
progToExec Program {stmt=s,input=input,output=output} = fst $ runState (stmtToExec s) $ varDeclsToTuples (input++output)

treeConcat :: ExecTree -> ExecTree -> ExecTree
treeConcat (Node e ts) t2=Node e (map (\t -> treeConcat t t2) ts)
treeConcat (Termination e) t2= Node e [t2]

stmtToExec :: Stmt -> State [(String,Type)] ExecTree
stmtToExec Skip = return $ Termination ESkip
stmtToExec (Assert e)           = do
                                    e' <- badExpr2goodExpr e
                                    return $ Termination (EAssert $ e')
stmtToExec (Assume e)           = do
                                    e' <- badExpr2goodExpr e
                                    return $ Termination (EAssume $ e')
stmtToExec (Assign s e)         = do 
                                    e' <- badExpr2goodExpr e
                                    return $ Termination (EAssign s $ e')
stmtToExec (AAssign s i e)      = do
                                    i' <- badExpr2goodExpr i
                                    e' <- badExpr2goodExpr e
                                    return $ Termination (EAAssign s i' e')
stmtToExec (DrefAssign s e)     = do
                                    e' <- badExpr2goodExpr e
                                    return $ Termination (EDrefAssign s e')
stmtToExec (Seq s1 s2)          = do 
                                    s1' <- stmtToExec s1
                                    s2' <- stmtToExec s2
                                    return $ treeConcat s1' s2'
stmtToExec (IfThenElse e s1 s2) = do
                                    e' <- badExpr2goodExpr e
                                    s1'<- stmtToExec s1
                                    s2'<- stmtToExec s2
                                    return $Node ESkip [Node (EAssume $ e') [s1'], Node (EAssume (Fix $ OpNeg $ e')) [s2']]
stmtToExec (While e s)      = do 
                                    e' <- badExpr2goodExpr e
                                    s'<- stmtToExec s
                                    return $ whileExec e' s'
  where
    whileExec cond body = Node ESkip [Node (EAssume cond) [treeConcat body (whileExec cond body),Termination (EAssume (Fix $ OpNeg cond))]]
stmtToExec (Block v s)          = do
                                    vars <- get
                                    mapM_ addVar $ varDeclsToTuples v
                                    stmtToExec s--Still has to replace all the occurences of the changed variables
stmtToExec (TryCatch _ _ _)     = undefined

replicateConcat :: Int -> [a] -> [a]
replicateConcat i xs = (concat $ replicate i xs)