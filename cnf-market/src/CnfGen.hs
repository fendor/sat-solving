{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module CnfGen where

import ClassyPrelude hiding (nub)
import Cnf
import Data.Semigroup
import Data.List (nub)
import Data.Ix (range)

type VarSymbol = String
type VarModifyer = String
type Sign = Bool

data SymLit = SymLit Sign VarSymbol [VarModifyer]
  deriving (Show)

newtype SymClause = SymClause [SymLit]
  deriving (Semigroup, Monoid, MonoFoldable, Show)
type instance Element SymClause = SymLit

type Range = (Int, Int)
type SymClauseQuantifyers = Map VarModifyer Range
data SymClauseFamily = SymClauseFamily SymClause SymClauseQuantifyers
  deriving (Show)


newtype SymFormula = SymFormula [SymClauseFamily]
  deriving (Semigroup, Monoid, Show)

-- INSTANCIATED TYPES

type SymClauseQuantifyersInstance = Map VarModifyer Int
data SymVarInstance = SymVarInstance VarSymbol [Int]
  deriving (Show, Eq, Ord)
data SymLitInstance = SymLitInstance Sign SymVarInstance
  deriving (Show, Eq, Ord)
newtype SymClauseInstance = SymClauseInstance [SymLitInstance]
  deriving (Show)

-- CREATION FUNCTIONS

plainSymLit :: VarSymbol -> SymLit
plainSymLit v = modifiedLit v []

modifiedLit :: VarSymbol -> [VarModifyer] -> SymLit
modifiedLit = SymLit True

negateLit :: SymLit -> SymLit
negateLit (SymLit s v m) = SymLit (not s) v m

unitClause :: SymLit -> SymClause
unitClause v = SymClause [v]

singleClause :: SymClause -> SymFormula
singleClause c = SymFormula [SymClauseFamily c mempty]

-- GENERATING FUNCTIONS

varSymbolsOfClause :: SymClause -> [VarSymbol]
varSymbolsOfClause (SymClause ls) = nub . map (\(SymLit _ s _) -> s) $ ls

varSymbolsOfClauseFamily :: SymClauseFamily -> [VarSymbol]
varSymbolsOfClauseFamily (SymClauseFamily c _) = varSymbolsOfClause c

varSymbolsOfFormula :: SymFormula -> [VarSymbol]
varSymbolsOfFormula (SymFormula cs) =
  nub . concatMap varSymbolsOfClauseFamily $ cs

modificationsToSymVar
  :: SymClauseQuantifyers -> SymLit -> Maybe (Set SymVarInstance)
modificationsToSymVar quants (SymLit _ v mods) = do -- Maybe monad
  ranges <- mapM (`lookup` quants) $ mods
  let allInstanciations =
        setFromList
          . map (SymVarInstance v)
          . cartesianProduct
          . map range
          $ ranges
  return allInstanciations

modificationsToSymVarInClause
  :: SymClauseQuantifyers -> SymClause -> Maybe (Set SymVarInstance)
modificationsToSymVarInClause quants (SymClause c) =
  map unions . mapM (modificationsToSymVar quants) $ c

modificationsToSymVarInClauseFamily
  :: SymClauseFamily -> Maybe (Set SymVarInstance)
modificationsToSymVarInClauseFamily (SymClauseFamily c quants) =
  modificationsToSymVarInClause quants c

modificationsToSymVarInFormula :: SymFormula -> Maybe (Set SymVarInstance)
modificationsToSymVarInFormula (SymFormula cs) =
  map unions . mapM modificationsToSymVarInClauseFamily $ cs

-- instanciateLit :: SymClauseQuantifyerInstance -> SymLit -> SymLitInstance
-- instanciateLit = undefined

instanciateClause :: SymClauseQuantifyers -> SymClause -> Maybe SymClauseInstance
instanciateClause quants (SymClause c) = error ""
  -- where
  --   allQuantifyerInstances =
  --     map mapFromList . cartesianProduct .
  --     map (\(q, r) -> [(q,x) | x <- range r ]) $ mapToList quants


genCnf :: SymFormula -> Cnf
genCnf cnfGen = Cnf clauses numVar numClauses []
 where
  allPlainVariables = modificationsToSymVarInFormula cnfGen
  plainVarLookuop   = [] -- mapFromList $ zip [1 ..] $ setToList allPlainVariables
  numVar            = length allPlainVariables
  numClauses        = 0
  clauses           = []

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct []         = []
cartesianProduct [xs      ] = map (: []) xs
cartesianProduct (xs : yss) = distribute xs (cartesianProduct yss)
  where distribute xs yss = concat (map (\x -> map (x :) yss) xs)
