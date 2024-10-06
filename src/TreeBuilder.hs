module TreeBuilder (
  ExecTree(..)
, ExecStmt(..)
, replaceVar
, badExpr2goodExpr
, progToExec
, progToExecMaxDepth) where

import GCLParser.GCLDatatype hiding (Expr(..))
import qualified GCLParser.GCLDatatype as P
import Data.Maybe (mapMaybe)
import Control.Monad.State
import Data.Functor.Foldable (Recursive (cata))
import Expr ( Expr, ExprF(..) )
import Data.Fix(Fix(Fix))
import Util (optionalError)

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
replace s e1 = cata f
  where
    f :: ExprF Expr -> Expr
    f (Var s' t) =  replaceVar s' e1 s t --foldExpr (defaultAlgebra {var=replaceVar s e})
    f e' = Fix e'

getVarStr :: P.Expr -> String
getVarStr (P.Var s) = s
getVarStr (P.RepBy e _ _) = getVarStr e
getVarStr e = error $ show e ++ " is not an array"

makeVar :: String -> Type -> Fix ExprF
makeVar n = Fix . Var n

makeQuantifier :: (String -> Expr -> ExprF Expr) -> String -> String -> Expr -> Expr
makeQuantifier q s s' e' = Fix $ q s' (replace s (makeVar s' $ PType PTInt) e')

replaceVar :: [Char] -> Expr -> [Char] -> Type -> Expr
replaceVar s1 e s2 t| s1==s2 = e
                    | otherwise = Fix $ Var s2 t 
                

badExpr2goodExpr :: P.Expr -> State [(String,Type)] Expr
badExpr2goodExpr (P.Var s)            = makeVar s <$> findType s
badExpr2goodExpr (P.LitI i)           = return $ Fix (LitI i)
badExpr2goodExpr (P.LitB b)           = return $ Fix (LitB b)
badExpr2goodExpr P.LitNull            = optionalError -- Pointer types.
badExpr2goodExpr (P.Parens e)         = badExpr2goodExpr e
badExpr2goodExpr (P.ArrayElem e1 e2)  = Fix <$> (ArrayElem <$> badExpr2goodExpr e1 <*> badExpr2goodExpr e2)
badExpr2goodExpr (P.OpNeg e)          = Fix . OpNeg <$> badExpr2goodExpr e
badExpr2goodExpr (P.BinopExpr op l r) = Fix <$> (BinopExpr op <$> badExpr2goodExpr l <*> badExpr2goodExpr r)
badExpr2goodExpr (P.Forall s e)       = makeQuantifier Forall s <$> addVar (s, PType PTInt) <*> badExpr2goodExpr e
badExpr2goodExpr (P.Exists s e)       = makeQuantifier Forall s <$> addVar (s, PType PTInt) <*> badExpr2goodExpr e
badExpr2goodExpr (P.SizeOf e)         = return $ Fix (SizeOf $ getVarStr e)
badExpr2goodExpr (P.RepBy e1 e2 e3)   = Fix <$> (RepBy <$> badExpr2goodExpr e1 <*> badExpr2goodExpr e2 <*> badExpr2goodExpr e3)
badExpr2goodExpr (P.Cond e1 e2 e3)    = Fix <$> (Cond <$> badExpr2goodExpr e1 <*> badExpr2goodExpr e2 <*> badExpr2goodExpr e3)
badExpr2goodExpr (P.NewStore _)       = optionalError -- Pointer types.
badExpr2goodExpr (P.Dereference _)    = optionalError -- Pointer types.


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
            Just t' -> t'

cut' :: Int -> ExecTree -> Maybe ExecTree
cut' d t@(Termination _) | d<0 = Nothing
                         | otherwise = Just t
cut' d (Node s ts)       | d<0 || null ts'= Nothing
                         | otherwise      = Just (Node s ts')
  where
    ts' = mapMaybe (cut' (d-1)) ts

varDeclsToTuples :: [VarDeclaration]-> [(String,Type)]
varDeclsToTuples = map (\(VarDeclaration s t)->(s,t))

progToExec :: Program -> ExecTree
progToExec Program {stmt=s,input=input,output=output} = evalState (stmtToExec s) (varDeclsToTuples (input++output))

treeConcat :: ExecTree -> ExecTree -> ExecTree
treeConcat (Node e ts) t2=Node e (map (`treeConcat` t2) ts)
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
                                    mapM_ addVar $ varDeclsToTuples v
                                    stmtToExec s--Still has to replace all the occurences of the changed variables
stmtToExec (TryCatch {})     = optionalError -- Exception handling.