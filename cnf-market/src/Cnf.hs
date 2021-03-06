module Cnf where

import ClassyPrelude hiding (maximumMay, maximum)
import Data.List (nub, maximum)
import Safe (maximumMay)


type Literal = Int

type Clause = [Literal]

data Cnf = Cnf
  { cnfClauses :: [Clause]
  , cnfNumVars :: Int
  , cnfNumClauses :: Int
  , cnfComment :: [Text]
  } deriving (Show, Eq, Read)

type Assignment = [Literal]

instance Semigroup Cnf where
  cnf1 <> cnf2 =
    Cnf
    (cnfClauses cnf1 ++ cnfClauses cnf2)
    (max (cnfNumVars cnf1) (cnfNumVars cnf2))
    (max (cnfNumClauses cnf1) (cnfNumClauses cnf2))
    (cnfComment cnf1 ++ cnfComment cnf2)

instance Monoid Cnf where
  mempty = Cnf [] 0 0 []

comment :: [Text] -> Cnf
comment = Cnf [] 0 0

commentLine :: Text -> Cnf
commentLine c = Cnf [] 0 0 [c]

cnfFromClauses :: [Clause] -> Cnf
cnfFromClauses clauses = Cnf clauses maxVar (length clauses) []
  where
    maxVar = maximum . mapMaybe maximumMay $ clauses

satisfies :: Assignment -> Clause -> Bool
satisfies assignment = any (`elem` assignment)
