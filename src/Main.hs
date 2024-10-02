{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Cli
import GCLParser.Parser
import Text.Pretty.Simple
    ( CheckColorTty(NoCheckColorTty),
      OutputOptions(OutputOptions),
      pPrintOpt,
      StringOutputStyle(EscapeNonPrintable) ) 
import Control.Monad.IO.Class (MonadIO)
import Util
import Data.Bool (bool)
import Control.Monad (when)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Fix (Fix (Fix))
import Z3Util (Expr, ExprF (..), expr2ast, isValid)
import GCLParser.GCLDatatype (PrimitiveType(..), Type (..), BinOp (Implication, GreaterThan))
import Z3.Monad (evalZ3)
import Control.Monad.State (StateT (runStateT))

main :: IO ()
main = do
  args@ArgData{ .. } <- getOptions
  ast <- parseGCLfile fileName
  startTime <- getCurrentTime
  (res, st, logs) <- runV (ReaderData args) placeholderVerify
  putStrLn $ bool "reject" "accept" res
  when showStats (print $ stats st)
  endTime <- getCurrentTime
  let timeUsed = diffUTCTime endTime startTime
  putStrLn $ "Total time used: " ++ show timeUsed
  print logs
  -- either (const $ putStrLn $ "Parse error in file " ++ fileName) (putStrLn . ppProgram2String) ast
  -- return ()

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt NoCheckColorTty (OutputOptions 2 120 True True 0 Nothing EscapeNonPrintable)

placeholderVerify :: V Bool
placeholderVerify = return True

-- forall x . x > 1 => x > 0
testExpr :: Expr
testExpr = Fix $ Forall "x" 
            (Fix $ BinopExpr Implication 
              (Fix $ BinopExpr GreaterThan (Fix $ Var "x" (PType PTInt)) (Fix $ LitI 1))   -- x > 1
              (Fix $ BinopExpr GreaterThan (Fix $ Var "x" (PType PTInt)) (Fix $ LitI 0)))  -- x > 0
            (PType PTInt)

testFull =  do
  (res, stat) <- evalZ3 $ runStateT (isValid =<< expr2ast testExpr) VState {
    stats = Stats {
      inspectedPaths  = 0
    , infeasiblePaths = 0
    , formulaSize     = 0
    }
  }
  print res
  print stat
