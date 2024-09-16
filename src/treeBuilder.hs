-- import GCLParser.Parser (parseGCLstring)
import GCLParser.GCLDatatype
import GHC.RTS.Flags (DebugFlags(stm))
import System.Environment (getArgs)


data ExecTree = Node ExecStmt [ExecTree] | Termination ExecStmt
data ExecStmt = ESkip
              | EAssert Expr
              | EAssume Expr
              | EAssign     String           Expr
              | EAAssign    String           Expr   Expr
              | EDrefAssign String           Expr

progToExec :: Program -> ExecTree
progToExec Program {stmt=s} =undefined

treeConcat :: ExecTree -> ExecTree -> ExecTree
treeConcat (Node e ts) t2=Node e (map (\t -> treeConcat t t2) ts)
treeConcat (Termination e) t2= Node e [t2]

stmtToExec :: Stmt -> ExecTree
stmtToExec Skip = Termination ESkip
stmtToExec (Assert e)= Termination (EAssert e)
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