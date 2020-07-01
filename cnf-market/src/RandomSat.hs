module RandomSat where

import ClassyPrelude
import Cnf
import Data.List (nubBy)
import System.Random

data RandomCnfConfig = RandomCnfConfig
  { randomConfigClauseLength :: Int
  , randomConfigNumVars :: Int
  , randomConfigNumClauses :: Int
  , randomConfigComment :: [Text]
  } deriving (Eq, Show)

-- create a random assignment
randomAssignment :: RandomGen gen => RandomCnfConfig -> gen -> Assignment
randomAssignment RandomCnfConfig { randomConfigNumVars } gen =
  zipWith (\v p -> if p then v else -v) [1 .. randomConfigNumVars] $ randoms gen

-- create a random Cnf formula that satisfies a given assignment
randomSatCnf :: RandomGen gen => Assignment -> RandomCnfConfig -> gen -> Cnf
randomSatCnf assignment (cnfConfig@RandomCnfConfig {..}) gen =
  cnfFromClauses clauses ++ comment randomConfigComment ++ commentLine
    ("seeded solution: " ++ unwords (map tshow assignment))
 where
  clauses =
    take randomConfigNumClauses
      . filter (satisfies assignment)
      . map (randomClause cnfConfig)
      $ randomGens gen

-- create a random Cnf formula
randomCnf :: RandomGen gen => RandomCnfConfig -> gen -> Cnf
randomCnf (cnfConfig@RandomCnfConfig {..}) gen = cnfFromClauses clauses
  ++ comment randomConfigComment
 where
  clauses =
    take randomConfigNumClauses . map (randomClause cnfConfig) $ randomGens gen

-- create a random clause
randomClause :: RandomGen gen => RandomCnfConfig -> gen -> Clause
randomClause (cnfConfig@RandomCnfConfig { randomConfigClauseLength }) gen =
  sortOn abs
    . take randomConfigClauseLength
    . nubBy (\x y -> abs x == abs y)
    . map (randomLiteral cnfConfig)
    $ randomGens gen

-- random literal
randomLiteral :: RandomGen gen => RandomCnfConfig -> gen -> Literal
randomLiteral RandomCnfConfig { randomConfigNumVars, randomConfigClauseLength } gen
  = let (var , gen') = randomR (1, randomConfigNumVars) gen
        (sign, _   ) = random gen'
    in  if sign then var else -var

-- creates an infinite stream of random generators split from given one
randomGens :: RandomGen gen => gen -> [gen]
randomGens gen = let (g1, g2) = split gen in g1 : randomGens g2
