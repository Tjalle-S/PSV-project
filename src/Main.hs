{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Cli
import GCLParser.Parser
import Text.Pretty.Simple
    ( CheckColorTty(NoCheckColorTty),
      OutputOptions(OutputOptions),
      pPrintOpt,
      StringOutputStyle(EscapeNonPrintable), pPrintStringOpt ) 
import Control.Monad.IO.Class (MonadIO (..))
import Util
import Control.Monad (when)
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Z3Util (expr2ast, getValidityCounterExample)
import GCLParser.GCLDatatype (PrimitiveType(..), Type (..), BinOp (Implication, GreaterThan))
import Z3.Monad (evalZ3, assert, getModel, showModel, mkNot, MonadZ3)
import Control.Monad.State (StateT (runStateT), MonadState)
import Expr (Expr (..), prettyishPrintExpr)
import WLP (makeWLPs)
import TreeBuilder (progToExec)

main :: IO ()
main = do
  args@ArgData{ .. } <- getOptions
  ast <- parseGCLfile fileName
  (_, timeUsed) <- withTimer $ do
    let wlp = case ast of
              Left  _err -> error "Parse error"
              Right ast' -> makeWLPs (LitB True) (progToExec ast')

    putStrLn $ unlines (map prettyishPrintExpr wlp) -- For debugging only. Be careful, as this prints all paths, even if not inspected.

    (res, st, logs) <- runV (ReaderData args) (testAllPaths wlp)
    putStr res

    when showStats $ do 
      pPrint (stats st)
      print logs
  putStrLn $ "Total time used: " ++ show timeUsed

-- | Keeps verifying program paths until an incorrect has been found.
testAllPaths :: (MonadZ3 m, MonadState VState m) => [Expr] -> m String
testAllPaths []     = return "Accept\n\n"
testAllPaths (e:es) = do
  incrNumPaths
  res <- getValidityCounterExample =<< expr2ast e
  case res of
    Nothing -> testAllPaths es                                            -- No counterexample found, check next WLP.
    Just ex -> return $ unlines ["Reject\n", "Variable assignments:", ex] -- Counterexample found, stop here.

printOptions :: OutputOptions
printOptions = OutputOptions 2 120 True True 0 Nothing EscapeNonPrintable

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt NoCheckColorTty printOptions

pPutStrLn :: MonadIO m => String -> m ()
pPutStrLn = pPrintStringOpt NoCheckColorTty printOptions

withTimer :: MonadIO m => m a -> m (a, NominalDiffTime)
withTimer ma = do
  startTime <- liftIO getCurrentTime
  a         <- ma
  endTime   <- liftIO getCurrentTime
  return (a, diffUTCTime endTime startTime)

-- forall x . x > 1 => x > 0
testExpr :: Expr
testExpr = Forall "x" 
            (BinopExpr Implication 
              (BinopExpr GreaterThan (Var "x" (PType PTInt)) (LitI 0))   -- x > 1
              (BinopExpr GreaterThan (Var "x" (PType PTInt)) (LitI 1)))  -- x > 0

testSatisfiable ast = maybe (return "No model") showModel . snd =<< ((assert =<< mkNot ast) >> getModel)

testFull = do
  (model, stat) <- evalZ3 $ runStateT (testSatisfiable =<< expr2ast testExpr) VState {
    stats = Stats {
      inspectedPaths  = 0
    , infeasiblePaths = 0
    , formulaSize     = 0
    }
  }
  putStrLn model
  pPrint stat
