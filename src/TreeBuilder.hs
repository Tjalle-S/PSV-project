module TreeBuilder (
  replace
, badExpr2goodExpr
, progToExec
, progToExecMaxDepth
, simplifyTree
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


both :: (a->b) -> (a,a) -> (b,b)
both f (x,y) = (f x,f y)

type Decl  = (String, Type)
type Decls = [Decl]

makeUnique :: String -> State Decls String
makeUnique s= do
                    vars <- get
                    let names =map fst vars  in
                      if s `elem` names
                        then makeUnique (s++"\'")
                      else
                        return s

findType :: String -> State Decls Type
findType s = do
                vars <- get
                case lookup s vars of
                  Just t -> return t
                  Nothing -> error $ "Variable "++s++" does not exist."
addVar :: Decl -> State Decls Decl
addVar (s,t) = do
                  s' <- makeUnique s
                  vars <- get
                  put ((s',t):vars)
                  return (s',t)

getVarStr :: P.Expr -> String
getVarStr (P.Var s) = s
getVarStr (P.RepBy e _ _) = getVarStr e
getVarStr e = error $ show e ++ " is not an array"

makeQuantifier :: (String -> Expr -> Expr) -> String -> String -> Expr -> Expr
makeQuantifier q s s' e' = q s' (replace s (Var s' $ PType PTInt) e')

badExpr2goodExpr :: P.Expr -> State Decls Expr
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

simplifyExpr :: Expr -> Expr
simplifyExpr = cata f 
  where
    f (OpNegF (OpNeg e)) = e

    f (BinopExprF And (LitB True) (LitB True)) = LitB True
    f (BinopExprF And (LitB True) r) = r 
    f (BinopExprF And l (LitB True)) = l 
    f (BinopExprF And (LitB False) _) = LitB False
    f (BinopExprF And _ (LitB False)) = LitB False
    f (BinopExprF And l r)  | l == r = l
                            | otherwise = BinopExpr And l r
    
    f (BinopExprF Or (LitB True) _) = LitB True
    f (BinopExprF Or _ (LitB True)) = LitB True
    f (BinopExprF Or (LitB False) r) = r 
    f (BinopExprF Or l (LitB False)) = l
    f (BinopExprF Or l r) | l == r = l
                          | otherwise = BinopExpr Or l r

    -- implications to disjunctive normal form (DNF)
    f (BinopExprF Implication l r) = simplifyExpr $ BinopExpr Or (OpNeg  l) r  

    f (BinopExprF LessThan (LitI l) (LitI r)) | l < r     = LitB True
                                              | otherwise = LitB False 
    f (BinopExprF LessThan l r)               | l == r    = LitB False
                                              | otherwise = BinopExpr LessThan l r       

    f (BinopExprF LessThanEqual (LitI l) (LitI r))  | l <= r    = LitB True
                                                    | otherwise = LitB False
    f (BinopExprF LessThanEqual l r)                | l == r    = LitB True
                                                    | otherwise = BinopExpr LessThanEqual l r
                                                      
    f (BinopExprF GreaterThan (LitI l) (LitI r))  | l > r     = LitB True
                                                  | otherwise = LitB False   
    f (BinopExprF GreaterThan l r)                | l == r    = LitB False
                                                  | otherwise = BinopExpr GreaterThan l r     

    f (BinopExprF GreaterThanEqual (LitI l) (LitI r)) | l >= r    = LitB True
                                                      | otherwise = LitB False
    f (BinopExprF GreaterThanEqual l r)               | l == r    = LitB True
                                                      | otherwise = BinopExpr GreaterThanEqual l r

    f (BinopExprF Equal (LitI l) (LitI r))  | l == r    = LitB True
                                            | otherwise = LitB False
    f (BinopExprF Equal l r)                | l == r    = LitB True
                                            | otherwise = BinopExpr Equal l r

    f (BinopExprF Plus (LitI l) (LitI r)) = LitI (l+r)
    -- Cases without parentheses
    f (BinopExprF Plus (LitI b) (BinopExpr Plus (LitI l) r)) = BinopExpr Plus (LitI (l+b)) r
    f (BinopExprF Plus (LitI b) (BinopExpr Plus l (LitI r))) = BinopExpr Plus (LitI (r+b)) l
    f (BinopExprF Plus (BinopExpr Plus (LitI l) r) (LitI b)) = BinopExpr Plus (LitI (l+b)) r
    f (BinopExprF Plus (BinopExpr Plus l (LitI r)) (LitI b)) = BinopExpr Plus (LitI (r+b)) l

    f (BinopExprF Plus (LitI b) (BinopExpr Minus (LitI l) r)) = BinopExpr Minus (LitI (l+b)) r
    f (BinopExprF Plus (LitI b) (BinopExpr Minus l (LitI r))) = BinopExpr Plus (LitI (b-r)) l
    f (BinopExprF Plus (BinopExpr Minus (LitI l) r) (LitI b)) = BinopExpr Minus (LitI (l+b)) r
    f (BinopExprF Plus (BinopExpr Minus l (LitI r)) (LitI b)) = BinopExpr Plus (LitI (b-r)) l

    f (BinopExprF Minus (LitI l) (LitI r)) = LitI (l-r)
    -- Cases without parentheses
    f (BinopExprF Minus (LitI b) (BinopExpr Minus (LitI l) r)) = BinopExpr Minus (LitI (b-l)) r
    f (BinopExprF Minus (LitI b) (BinopExpr Minus l (LitI r))) = BinopExpr Minus (LitI (b-r)) l
    f (BinopExprF Minus (BinopExpr Minus (LitI l) r) (LitI b)) = BinopExpr Minus (LitI (l-b)) r
    f (BinopExprF Minus (BinopExpr Minus l (LitI r)) (LitI b)) = BinopExpr Minus l (LitI (b+r)) 

    f (BinopExprF Minus (LitI b) (BinopExpr Plus (LitI l) r)) = BinopExpr Plus (LitI (b-l)) r
    f (BinopExprF Minus (LitI b) (BinopExpr Plus l (LitI r))) = BinopExpr Minus (LitI (b+r)) l
    f (BinopExprF Minus (BinopExpr Plus (LitI l) r) (LitI b)) = BinopExpr Plus r (LitI (l-b))
    f (BinopExprF Minus (BinopExpr Plus l (LitI r)) (LitI b)) = BinopExpr Plus l (LitI (r-b))


    f (BinopExprF Multiply (LitI l) (LitI r)) = LitI (l*r)

    f (BinopExprF Divide (LitI l) (LitI r)) | r == 1    = LitI l
                                            | l == 0    = LitI l
                                            | otherwise = BinopExpr Divide (LitI l) (LitI r)

    f e = embed e

simplifyTree :: ExecTree -> ExecTree
simplifyTree (Node e t) = Node (simplifyStmt e) (map simplifyTree t)
simplifyTree (Termination e) = Termination (simplifyStmt e)

simplifyStmt :: ExecStmt -> ExecStmt
simplifyStmt (EAssert e) = EAssert (simplifyExpr e)
simplifyStmt (EAssume e) = EAssume (simplifyExpr e)
simplifyStmt (EAssign s e) = EAssign s (simplifyExpr e)
simplifyStmt (EAAssign s e1 e2) = EAAssign s (simplifyExpr e1) (simplifyExpr e2)
simplifyStmt (EDrefAssign s e) = EDrefAssign s (simplifyExpr e)
simplifyStmt s = s


progToExecMaxDepth :: Bool -> Int -> Program -> ExecTree
progToExecMaxDepth checkInv d  = cut d . progToExec checkInv

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
cut' d (LoopInv t1 t2 from to) = do
                                   t1' <- cut' d t1
                                   t2' <- cut' d t2
                                   let (rt1,rt2) = (replaceVarsTree t1' from to, replaceVarsTree t2' from to)
                                   return $ Node ESkip [rt1,rt2]
--TODO: Make it look better 

varDeclsToTuples :: [VarDeclaration]-> Decls
varDeclsToTuples = map (\(VarDeclaration s t)->(s,t))

progToExec :: Bool -> Program -> ExecTree
progToExec checkInv Program {stmt = s, input = i, output = o} = evalState (stmtToExec checkInv s) (varDeclsToTuples $ i ++ o)

treeConcat :: ExecTree -> ExecTree -> ExecTree
treeConcat (Node e ts)     t2 = Node e (map (`treeConcat` t2) ts)
treeConcat (Termination e) t2 = Node e [t2]
treeConcat (LoopInv t1 t2 from to) t3 = LoopInv t1 (treeConcat t2 t3) from to

makeTerminate :: (Expr -> ExecStmt) -> P.Expr -> State Decls ExecTree
makeTerminate s e = Termination . s <$> badExpr2goodExpr e

stmtToExec :: Bool -> Stmt -> State Decls ExecTree
stmtToExec _ Skip             = return $ Termination ESkip
stmtToExec _ (Assert e)       = makeTerminate EAssert     e
stmtToExec _ (Assume e)       = makeTerminate EAssume     e
stmtToExec _ (Assign s e)     = makeTerminate (EAssign s) e
stmtToExec _ (AAssign s i e)  = Termination <$> (EAAssign s <$> badExpr2goodExpr i <*> badExpr2goodExpr e)
stmtToExec _ (DrefAssign s e) = makeTerminate (EDrefAssign s) e

stmtToExec checkInv (Seq (Assert inv) (While c b))          = stmtToExec checkInv $ Seq (Assert inv) (Seq (While c b) Skip)
stmtToExec True     (Seq (Assert inv) (Seq (While c b) _T)) = loopInvariant <$> badExpr2goodExpr inv <*> badExpr2goodExpr c <*> stmtToExec True b <*> stmtToExec True _T <*> assigned <*> (mapM addVar =<< assigned)
  where
    assigned :: State Decls Decls
    assigned = gets (getAssigned b)

    getAssigned (Seq s1 s2)          vars = union ( getAssigned s1 vars) (getAssigned s2 vars)
    getAssigned (Assign str _)       vars = assign str vars
    getAssigned (AAssign str _ _)    vars = assign str vars
    getAssigned (DrefAssign str _)   vars = assign str vars
    getAssigned (IfThenElse _ s1 s2) vars = union (getAssigned s1 vars) (getAssigned s2 vars)
    getAssigned (While _ s)          vars = getAssigned s vars
    getAssigned (Block _ s)          vars = getAssigned s vars
    getAssigned _ _                       = []
    assign :: String -> Decls -> Decls
    assign str var = case lookup str var of
                          Just t -> [(str,t)]
                          Nothing -> error $ "Variable "++str++" does not exist."

        -- For the loop invariant, we want to be able to see when an assert comes before
        -- a while loop. For this we assume that statement sequences are in normal
        -- form.
stmtToExec _        (Seq (Seq _ _) _)    = error "is not normalized"
stmtToExec checkInv (Seq s1 s2)          = treeConcat <$> stmtToExec checkInv s1 <*> stmtToExec checkInv s2
stmtToExec checkInv (IfThenElse e s1 s2) = do
                                    e' <- badExpr2goodExpr e
                                    s1'<- stmtToExec checkInv s1
                                    s2'<- stmtToExec checkInv s2
                                    return $ Node ESkip [Node (EAssume e') [s1'], Node (EAssume $ OpNeg e') [s2']]
stmtToExec checkInv (While e s)          = whileExec <$> badExpr2goodExpr e <*> stmtToExec checkInv s
  where
    whileExec cond body = Node ESkip [Node (EAssume cond) [treeConcat body (whileExec cond body)],Termination (EAssume $ OpNeg cond)]
stmtToExec checkInv (Block v s)          = do
                                    v' <- mapM addVar (varDeclsToTuples v)
                                    t  <- stmtToExec checkInv s
                                    return $ replaceVarsTree t (varDeclsToTuples v) v'
stmtToExec _ (TryCatch {})     = optionalError -- Exception handling.
--TODO: Make Tree fold
replaceVarsTree :: ExecTree -> Decls -> Decls -> ExecTree
replaceVarsTree (Node s c)      v by = Node        (replaceVarsStmt s v by) (map (\t -> replaceVarsTree t v by) c)
replaceVarsTree (Termination s) v by = Termination (replaceVarsStmt s v by)
replaceVarsTree (LoopInv t1 t2 from to) v by = LoopInv t1' t2' from to
  where
    (t1',t2') = both (\t -> replaceVarsTree t v' by') (t1,t2)
    (v',by')  = unzip $ filter (\(var,_) -> not (var `elem` from)) $ zip v by

replaceVarsStmt :: ExecStmt -> Decls -> Decls -> ExecStmt
replaceVarsStmt ESkip              _ _  = ESkip
replaceVarsStmt (EAssert e)        v by = EAssert     (replaceVars e v by)
replaceVarsStmt (EAssume e)        v by = EAssume     (replaceVars e v by)
replaceVarsStmt (EAssign s e)      v by = EAssign     (replaceVarsStr s v by) (replaceVars e v by)
replaceVarsStmt (EAAssign s e1 e2) v by = EAAssign    (replaceVarsStr s v by) (replaceVars e1 v by) (replaceVars e2 v by)
replaceVarsStmt (EDrefAssign s e)  v by = EDrefAssign (replaceVarsStr s v by) (replaceVars e v by)

replaceVarsStr :: String -> Decls -> Decls -> String
replaceVarsStr s v by = fst $ replaceVarsStrT (s,undefined) v by

replaceVarsStrT :: Decl -> Decls -> Decls -> Decl
replaceVarsStrT d v by = foldl replaceVarsStrTSingle d (zip v by)

replaceVarsStrTSingle :: Decl -> (Decl, Decl) -> Decl
replaceVarsStrTSingle (s1, t1) ((s2, _),(s3, t3)) | s1 == s2  = (s3,t3)
                                                  | otherwise = (s1,t1)
replaceVars:: Expr -> Decls -> Decls -> Expr
replaceVars e v by = cata f e
  where
    f (VarF s t)  = let (s', t') = replaceVarsStrT (s,t) v by
                    in  Var s' t'
    f (SizeOfF s) = SizeOf (replaceVarsStr s v by)
    f e'          = embed e'

loopInvariant::Expr->Expr->ExecTree->ExecTree->Decls->Decls->ExecTree
loopInvariant inv c b _T assigned newvars = Node (EAssert inv) [LoopInv validInv rest assigned newvars]
  where
      --initInv = Termination $ EAssert inv
      validInv = foldr1 treeConcat [Termination $ EAssume (BinopExpr And inv c),b, Termination $  EAssert inv]
      --validInv'= ReplaceVars validInv assigned newvars
      rest = Node (EAssume (BinopExpr And inv (OpNeg c))) [_T]
      --rest' = ReplaceVars rest assigned newvars

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
    f e@(SizeOfF n)| n == var  = case by of
                                  (Var byn _) -> SizeOf byn
                                  _ -> error "Can only replace the contents of a sizeOf expression with a variable"
                   | otherwise = embed e
    f e                        = embed e
