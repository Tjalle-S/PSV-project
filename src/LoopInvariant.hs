{-# LANGUAGE FlexibleContexts #-}
module LoopInvariant (removeLoops) where
import GCLParser.GCLDatatype
import TreeBuilder (progToExecMaxDepth)
import WLP (calcWLP)
import Z3.Monad
import Util
--import Expr (Expr(BinopExpr))

type FStmt  = (MonadZ3 m,MonadG m) =>m Stmt -> m Stmt

removeLoops :: Program -> IO Program
removeLoopsStmt :: [VarDeclaration] -> Stmt -> (FStmt,Maybe (Expr,Stmt),Maybe Expr)
combineStartEnd ::  (FStmt,Maybe (Expr,Stmt),Maybe Expr) -> IO Stmt
combineStartEnd (s,Just (c,b),Just e )= Seq (While c b) <$> s (return $ Assert e)
combineStartEnd (s,Just (c,b),Nothing)= Seq (While c b) <$> s (return Skip)
combineStartEnd (s,Nothing   ,Just e )= s $ return $ Assert e
combineStartEnd (s,Nothing   ,Nothing)= s $ return Skip

removeLoops p@(Program {stmt = s, input = i, output = o}) = (\s''->p{stmt=s''})<$>s'
  where
    s'=combineStartEnd $ removeLoopsStmt (i++o) s


                                             --Loop at start,Assert at end
removeLoopsStmt _ s@Skip                =((Seq s<$>),   Nothing,      Nothing)
removeLoopsStmt _   (Assert e)          =(id,      Nothing,      Just e)
removeLoopsStmt _ s@(Assume _)          =((Seq s<$>),   Nothing,      Nothing)
removeLoopsStmt _ s@(Assign _ _)        =((Seq s<$>),   Nothing,      Nothing)
removeLoopsStmt _ s@(AAssign {})        =((Seq s<$>),   Nothing,      Nothing)
removeLoopsStmt _ s@(DrefAssign _ _)    =((Seq s<$>),   Nothing,      Nothing)
removeLoopsStmt v   (Seq s1 s2)         = case removeLoopsStmt v s1 of
                                             (s1',w,Just e) -> case removeLoopsStmt v s2 of
                                                                 (s2',Just (c,b),a) -> (s1'.inv.s2',w,a)
                                                                                         where inv s_ = b' >>= (\b'' -> convertLoop v e c b'' s_)
                                                                                               b' = combineStartEnd $ removeLoopsStmt v b
                                                                 (s2',Nothing   ,a) -> (s1'.(Seq (Assert e)<$>).s2',w,a)
                                             (s1',w,Nothing)-> case removeLoopsStmt v s2 of
                                                                 (s2',Just (c,b),a) -> (s1'.(Seq .While c<$>b'<*>).s2',w,a)
                                                                                          where b' = combineStartEnd $ removeLoopsStmt v b
                                                                 (s2',Nothing   ,a) -> (s1'.s2',w,a)
removeLoopsStmt v   (IfThenElse e s1 s2)=((Seq<$>s'<*>),   Nothing,      Nothing)
                    where
                      s'= IfThenElse e<$>s1'<*>s2'
                      s1'= combineStartEnd $ removeLoopsStmt v s1
                      s2'= combineStartEnd $ removeLoopsStmt v s2
removeLoopsStmt _   (While e s_)        =(id,       Just (e,s_),    Nothing)
removeLoopsStmt v   (Block v_ s_)       =((Seq<$>s'<*>),Nothing,Nothing)
                    where
                      s'  = Block v_<$>s_'
                      s_' = combineStartEnd $ removeLoopsStmt (v++v_) s_
removeLoopsStmt _ (TryCatch {}) = undefined
convertLoop :: [VarDeclaration] -> Expr -> Expr -> Stmt -> FStmt
convertLoop v i cond body = if  {-{cond^I} body {I}-}undefined then
                              (Seq (Assert i)<$>).(Block v<$>).(Seq (Assume $ BinopExpr And i $ OpNeg cond)<$>)
                            else
                              (Seq (Assert i)<$>).(Seq (While cond body)<$>)
invariantValid :: (MonadZ3 m,MonadG m) => [VarDeclaration] -> Expr -> Expr -> Stmt -> m Bool
invariantValid v i cond body = calcWLP t  
  where t = progToExecMaxDepth 100 (Program {stmt=s, input=v,output=[]})
        s = Seq (Assume (BinopExpr And i cond)) $ Seq body (Assert i)