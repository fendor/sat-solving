module UnsatCore where

import Cnf
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (<|), (|>))
import MiniSat
import Control.Monad
import RandomSat
import System.Random

type LiteralMap = Map Lit Literal
type VisitedMap = IntMap.IntMap Lit

findUnsatCore :: Cnf -> IO (Maybe (Seq Clause))
findUnsatCore initCnf = do
  let clauses = Seq.fromList $ cnfClauses initCnf
  sat <- isSat clauses
  if sat
    then pure Nothing
    else Just <$> loop Seq.empty clauses
  where
    loop core clauses
      | Seq.null clauses = pure core
      | otherwise = do
          let e :<| clauses' = clauses
          isSat (core <> clauses')
            >>= \case
            False -> loop core clauses'
            True -> loop (e <| core) clauses'

isSat :: Foldable t => t Clause -> IO Bool
isSat clauses = withNewSolverAsync $ \solver -> do
  (sat,_,_) <- solveCnf solver clauses
  pure sat

solveCnf :: Foldable t => Solver -> t Clause -> IO (Bool, LiteralMap, VisitedMap)
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

myRandomCnf :: Int -> Int -> Int -> IO Cnf
myRandomCnf clauseLen vars varsPerClause = do
  gen <- getStdGen
  pure $ randomCnf (RandomCnfConfig clauseLen vars varsPerClause []) gen

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