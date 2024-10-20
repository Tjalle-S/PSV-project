module TreeBuilder (
  replace
, badExpr2goodExpr
, progToExec
, progToExecMaxDepth
, stmtToExec) where --The last one should be temporarily

import GCLParser.GCLDatatype hiding (Expr(..))
import qualified GCLParser.GCLDatatype as P
import Data.Maybe (mapMaybe)
import Data.List (union)
import Control.Monad.State
import Data.Functor.Foldable (Recursive (cata), Corecursive (embed))
import Expr ( Expr (..), ExprF(..) )
import Util (optionalError)
import Statement (ExecTree(..), ExecStmt(..), ExecTree(..))

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
addVar :: (String,Type) -> State [(String,Type)] (String,Type)
addVar (s,t) = do
                  s' <- makeUnique s
                  vars <- get
                  put ((s',t):vars)
                  return (s',t)
--copied from WLP
-- replace :: String -> Expr -> Expr -> Expr
-- replace s e1 = cata f
--   where
--     f :: ExprF Expr -> Expr
--     f (VarF s' t) =  replaceVar s' e1 s t --foldExpr (defaultAlgebra {var=replaceVar s e})
--     f e' = embed e'

getVarStr :: P.Expr -> String
getVarStr (P.Var s) = s
getVarStr (P.RepBy e _ _) = getVarStr e
getVarStr e = error $ show e ++ " is not an array"

makeQuantifier :: (String -> Expr -> Expr) -> String -> String -> Expr -> Expr
makeQuantifier q s s' e' = q s' (replace s (Var s' $ PType PTInt) e')

badExpr2goodExpr :: P.Expr -> State [(String,Type)] Expr
badExpr2goodExpr (P.Var s)            = Var s <$> findType s
badExpr2goodExpr (P.LitI i)           = return (LitI i)
badExpr2goodExpr (P.LitB b)           = return (LitB b)
badExpr2goodExpr (P.ArrayElem e1 e2)  = ArrayElem <$> badExpr2goodExpr e1 <*> badExpr2goodExpr e2
badExpr2goodExpr (P.OpNeg e)          = OpNeg <$> badExpr2goodExpr e
badExpr2goodExpr (P.BinopExpr op l r) = BinopExpr op <$> badExpr2goodExpr l <*> badExpr2goodExpr r
badExpr2goodExpr (P.Forall s e)       = makeQuantifier Forall s <$> (fst <$> addVar (s, PType PTInt)) <*> badExpr2goodExpr e
badExpr2goodExpr (P.Exists s e)       = makeQuantifier Exists s <$> (fst <$> addVar (s, PType PTInt)) <*> badExpr2goodExpr e
badExpr2goodExpr (P.SizeOf e)         = return (SizeOf $ getVarStr e)
badExpr2goodExpr (P.RepBy e1 e2 e3)   = RepBy <$> badExpr2goodExpr e1 <*> badExpr2goodExpr e2 <*> badExpr2goodExpr e3
badExpr2goodExpr (P.Parens e)         = badExpr2goodExpr e
badExpr2goodExpr (P.Cond e1 e2 e3)    = Cond <$> badExpr2goodExpr e1 <*> badExpr2goodExpr e2 <*> badExpr2goodExpr e3
badExpr2goodExpr (P.NewStore _)       = optionalError -- Pointer types.
badExpr2goodExpr (P.Dereference _)    = optionalError -- Pointer types.
badExpr2goodExpr P.LitNull            = optionalError -- Pointer types.

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
progToExec Program {stmt = s, input = i, output = o} = evalState (stmtToExec s) (varDeclsToTuples $ i ++ o)

treeConcat :: ExecTree -> ExecTree -> ExecTree
treeConcat (Node e ts)     t2 = Node e (map (`treeConcat` t2) ts)
treeConcat (Termination e) t2 = Node e [t2]

makeTerminate :: (Expr -> ExecStmt) -> P.Expr -> State [(String, Type)] ExecTree
makeTerminate s e = Termination . s <$> badExpr2goodExpr e

stmtToExec :: Stmt -> State [(String,Type)] ExecTree
stmtToExec Skip                 = return $ Termination ESkip
stmtToExec (Assert e)           = makeTerminate EAssert     e
stmtToExec (Assume e)           = makeTerminate EAssume     e
stmtToExec (Assign s e)         = makeTerminate (EAssign s) e
stmtToExec (AAssign s i e)      = Termination <$> (EAAssign s <$> badExpr2goodExpr i <*> badExpr2goodExpr e)
stmtToExec (DrefAssign s e)     = makeTerminate (EDrefAssign s) e

stmtToExec (Seq (Assert inv) (Seq (While c b) _T)) = loopInvariant <$> (badExpr2goodExpr inv) <*> (badExpr2goodExpr c) <*> stmtToExec b <*> stmtToExec _T <*> assigned <*> (mapM addVar =<< assigned) <*> (mapM addVar =<< assigned)
  where assigned :: State [(String,Type)] [(String,Type)]
        assigned = getAssigned b <$> get
        getAssigned (Seq s1 s2) vars = union (getAssigned s1 vars) (getAssigned s2 vars)
        getAssigned (Assign str _) vars = assign str vars
        getAssigned (AAssign str _ _) vars = assign str vars
        getAssigned (DrefAssign str _) vars = assign str vars
        getAssigned _ _ = []
        assign :: String -> [(String,Type)] -> [(String,Type)]
        assign str var = case lookup str var of
                              Just t -> [(str,t)]
                              Nothing -> error $ "Variable "++str++" does not exist."

				-- For the loop invariant, we want to be able to see when an assert comes before
				-- a while loop. For this we assume that statement sequences are in normal
				-- form.
stmtToExec (Seq (Seq _ _) _)    = error "is not normalized"
stmtToExec (Seq s1 s2)          = treeConcat <$> stmtToExec s1 <*> stmtToExec s2
stmtToExec (IfThenElse e s1 s2) = do
                                    e' <- badExpr2goodExpr e
                                    s1'<- stmtToExec s1
                                    s2'<- stmtToExec s2
                                    return $ Node ESkip [Node (EAssume e') [s1'], Node (EAssume $ OpNeg e') [s2']]
stmtToExec (While e s)          = whileExec <$> badExpr2goodExpr e <*> stmtToExec s
  where
    whileExec cond body = Node ESkip [Node (EAssume cond) [treeConcat body (whileExec cond body)],Termination (EAssume $ OpNeg cond)]
stmtToExec (Block v s)          = do
                                    v' <- mapM addVar (varDeclsToTuples v)
                                    t <- (stmtToExec s)
                                    return $ replaceVarsTree t (varDeclsToTuples v) v'
stmtToExec (TryCatch {})     = optionalError -- Exception handling.
--TODO: Make Tree fold
replaceVarsTree :: ExecTree -> [(String,Type)] -> [(String,Type)] -> ExecTree
replaceVarsTree (Node s c) v by = Node (replaceVarsStmt s v by) (map (\t -> replaceVarsTree t v by) c)
replaceVarsTree (Termination s) v by = Termination (replaceVarsStmt s v by)
replaceVarsStmt :: ExecStmt -> [(String,Type)] -> [(String,Type)] -> ExecStmt
replaceVarsStmt ESkip _ _ = ESkip
replaceVarsStmt (EAssert e) v by = EAssert $ replaceVars e v by
replaceVarsStmt (EAssume e) v by = EAssume $ replaceVars e v by
replaceVarsStmt (EAssign s e) v by = EAssign (replaceVarsStr s v by) (replaceVars e v by)
replaceVarsStmt (EAAssign s e1 e2) v by = EAAssign (replaceVarsStr s v by) (replaceVars e1 v by) (replaceVars e2 v by)
replaceVarsStmt (EDrefAssign s e) v by = EDrefAssign (replaceVarsStr s v by) (replaceVars e v by)
replaceVarsStr :: String -> [(String,Type)] -> [(String,Type)] -> String
replaceVarsStr s v by = fst $ replaceVarsStrT (s,undefined) v by -- 
replaceVarsStrT :: (String,Type) -> [(String,Type)] -> [(String,Type)] -> (String,Type)
replaceVarsStrT (s,t) v by = foldl replaceVarsStrTSingle (s,t) (zip v by)
replaceVarsStrTSingle :: (String,Type) -> ((String,Type) , (String,Type)) -> (String,Type)
replaceVarsStrTSingle (s1,t1) ((s2,t2),(s3,t3))  | s1==s2 = (s3,t3)
                                                 | otherwise = (s1,t1)
replaceVars:: Expr -> [(String,Type)] -> [(String,Type)] -> Expr
replaceVars e v by= cata f e
  where
    f (VarF s t) = Var s' t'
      where
        (s',t') = replaceVarsStrT (s,t) v by
    f (SizeOfF s) = SizeOf (replaceVarsStr s v by)
    f e = embed e

loopInvariant::Expr->Expr->ExecTree->ExecTree->[(String,Type)]->[(String,Type)]->[(String,Type)]->ExecTree
loopInvariant inv c b _T assigned newvars newnewvars  = foldr1 treeConcat [validInv',assumeInv,rest']
  where
      validInv = foldr1 treeConcat [Termination $ EAssume (BinopExpr And inv c),b, Termination $  EAssume inv]
      validInv'= replaceVarsTree validInv assigned newvars
      assumeInv = Termination (EAssert inv)
      rest = Node (EAssume (BinopExpr And inv (OpNeg c))) [_T]
      rest' = replaceVarsTree rest assigned newnewvars
      
--    replaceVw0wars Skip vars by = Skip
--    replaceVars (Assert e) ((v,t):vars) (b:by) = Assert $ embed (replace v undefined undefined)


                                    -- stmtToExec s--Still has to replace all the occurences of the changed variables

-- blockToExec :: [Expr -> Expr]

-- | Replace any occurrence of given variable by another expression.
--
-- @ replace x e Q @ is equivalent to Q[e/x].
replace :: String -> Expr -> Expr -> Expr
replace var by = cata f
  where
    f e@(VarF n _) | n == var  = by
                   | otherwise = embed e
    f e@(SizeOfF n)| n == var = case by of
                                  (Var byn _) -> embed $ SizeOfF byn
                                  otherwise -> error "Can only replace the contents of a sizeOf expression with a variable"
                   | otherwise = embed e
    f e = embed e
