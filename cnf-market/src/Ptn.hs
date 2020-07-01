module Ptn where

import ClassyPrelude
import Cnf

intRoot2 :: Int -> Maybe Int
intRoot2 n = if intRoot * intRoot == n then Just intRoot else Nothing
  where
    floatRoot = sqrt . fromIntegral $ n :: Double
    intRoot = round floatRoot

pTriplesUpTo :: Int -> [(Int, Int, Int)]
pTriplesUpTo m =
  [ (x, y, z)
  | x <- [1 .. m]
  , y <- [x .. min m (((x*x)-1) `div`2)]
  , let z2 = x * x + y * y
  , z2 <= m*m
  , z <- maybeToList $ intRoot2 z2
  ]

ptnCnf :: Int -> Cnf
ptnCnf m = Cnf clauses m (2 * length triples) []
 where
  triples = pTriplesUpTo m
  clauses = concatMap (\(x, y, z) -> [[x, y, z], [-x, -y, -z]]) triples
