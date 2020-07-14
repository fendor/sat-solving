{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module UnsatCore where

import Cnf
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Set (Set)
import MiniSat
import Control.Monad

type LiteralMap = Map Lit Literal
type VisitedMap = IntMap.IntMap Lit

findUnsatCore :: Cnf -> IO [Clause]
findUnsatCore initCnf = do
  let clauses = cnfClauses initCnf
  sat <- isSat clauses
  if sat
    then pure []
    else loop clauses
  where
    go (left, !dirty, []) = pure (left, dirty)
    go (left, !dirty, x:xs) = do
      isSat (left ++ xs)
        >>= \case
          True -> go (x:left, dirty, xs)
          False -> go (left, dirty || True, xs)

    loop oldClauses = do
      (newCnf, dirty) <- go ([], False, oldClauses)
      if not dirty
        then pure newCnf
        else loop newCnf

isSat :: [Clause] -> IO Bool
isSat clauses = withNewSolverAsync $ \solver -> do
  (sat,_,_) <- solveCnf solver clauses
  pure sat

solveCnf :: Solver -> [Clause] -> IO (Bool, LiteralMap, VisitedMap)
solveCnf solver clauses =  do
    (cm, lm) <- foldM (\(lm, visited) clause -> do
      (newClause, cm, lm) <-
        foldM
            (\(cl, lm, vm) lit -> toClause solver cl lm vm lit
            )
            ([], lm, visited)
            clause
      addClause solver newClause
      pure (cm, lm)
      ) (Map.empty, IntMap.empty) clauses
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

unsatFormula :: Cnf
unsatFormula = Cnf
  { cnfClauses = [[1], [-1], [1,2,3]]
  , cnfNumVars = 3
  , cnfNumClauses = 3
  , cnfComment = []
  }