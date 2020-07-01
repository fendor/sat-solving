module UnsatCore where

import Cnf
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import MiniSat
import Control.Monad (foldM)

type LiteralMap = Map Lit Literal
type VisitedMap = IntMap.IntMap Lit

findUnsatCore :: Cnf -> IO [Clause]
findUnsatCore cnf = withNewSolverAsync $ \solver -> do
    (sat, cm, lm) <- solveCnf solver cnf
    print cm
    print lm
    print sat
    pure []



solveCnf :: Solver -> Cnf -> IO (Bool, LiteralMap, VisitedMap)
solveCnf solver cnf =  do
    (cm, lm) <- foldM (\(lm, visited) clause -> do
      (newClause, cm, lm) <-
        foldM
            (\(cl, lm, vm) lit -> toClause solver cl lm vm lit
            )
            ([], lm, visited)
            clause
      addClause solver newClause
      pure (cm, lm)
      ) (Map.empty, IntMap.empty) (cnfClauses cnf)
    sat <- solve solver []
    pure (sat, cm, lm)
  where
  toClause :: Solver -> [Lit] -> LiteralMap -> VisitedMap -> Int -> IO ([Lit], LiteralMap, VisitedMap)
  toClause s cl lm vm l = do
    let absL = abs l
    (lit, lm, vm) <- case IntMap.lookup absL vm of
      Nothing -> do
        lit <- newLit s
        pure (lit, Map.insert lit absL lm, IntMap.insert absL lit vm)
      Just lit -> do
        pure (lit, lm, vm)

    let rLit = if l < 0
          then neg lit
          else lit

    pure (rLit: cl, lm, vm)

testdata :: Cnf
testdata = Cnf
  { cnfClauses = [[1,2,3,4], [-1, -3, 2], [-3,2]]
  , cnfNumVars = 4
  , cnfNumClauses = 3
  , cnfComment = []
  }
