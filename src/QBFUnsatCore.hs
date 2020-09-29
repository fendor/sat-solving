{-# LANGUAGE RecordWildCards #-}

module QBFUnsatCore where

import Cnf
import Data.Char (isDigit)
import Data.List (intercalate, isPrefixOf, zipWith4)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.Exit
import System.Process

findUnsatCore :: Cnf -> IO (Maybe (Seq Clause))
findUnsatCore cnf = do
  let reduction = reduceToQCir cnf
  let fp = "findUnsatCore.qcir"
  writeFile fp reduction
  (_exitCode, sout, _serr) <- readProcessWithExitCode "qfun" [fp] ""
  pure $ Just $ Seq.fromList $ resultToClauses $ last $ lines sout
  where
    resultToClauses :: String -> [Clause]
    resultToClauses s =
      let stringClauses = filter ("+" `isPrefixOf`) $ words s
          indices = map (subtract 1 . read . filter isDigit) stringClauses
       in map (cnfClauses cnf !!) indices

reduceToQCir :: Cnf -> String
reduceToQCir Cnf {..} =
  unlines
    [ "#QCIR-G14",
      "exists(" ++ intercalate ", " uVars ++ ")",
      "forall(" ++ intercalate ", " vVars ++ ")",
      "forall(" ++ intercalate ", " dVars ++ ")",
      "exists(" ++ intercalate ", " vpVars ++ ")",
      "output(Q)",
      unlines buildQ1,
      unlines buildDNotU,
      "minOneD = or(" ++ intercalate ", " dVars ++ ")",
      unlines buildMaxOneD,
      "oneD = and(minOneD, maxOneD)",
      unlines buildFdu,
      "Q2 = or(DNotU, -oneD, Fdu)",
      "Q = and(-Q1, Q2)"
    ]
  where
    vars = [1 .. cnfNumVars]
    clauseNums = [1 .. cnfNumClauses]
    vVars = map (("v" ++) . show) vars
    vpVars = map (("vp" ++) . show) vars
    uVars = map (("u" ++) . show) clauseNums
    dVars = map (("d" ++) . show) clauseNums
    buildQ1 :: [String]
    buildQ1 =
      (zipWith3 (\c u n -> n ++ " = or(-" ++ u ++ ", " ++ intercalate ", " (map getLiteral c) ++ ")") cnfClauses uVars clauseNames)
        ++ ["Q1 = and(" ++ intercalate ", " clauseNames ++ ")"]
      where
        clauseNames = map (("q1Clause" ++) . show) clauseNums
        getLiteral :: Int -> String
        getLiteral ind = (if ind < 0 then "-" else "") ++ (vVars !! (abs ind - 1))
    buildDNotU :: [String]
    buildDNotU = (zipWith3 (\d u n -> n ++ " = and(" ++ d ++ ", -" ++ u ++ ")") dVars uVars names) ++ ["DNotU = or(" ++ intercalate ", " names ++ ")"]
      where
        names = map (\x -> "d" ++ show x ++ "Notu" ++ show x) clauseNums
    buildMaxOneD :: [String]
    buildMaxOneD = (zipWith (++) names (combineDvars dVars)) ++ ["maxOneD = and(" ++ intercalate ", " names ++ ")"]
      where
        combineDvars :: [String] -> [String]
        combineDvars [] = []
        combineDvars (x : xs) = map (\y -> " = or(-" ++ x ++ ", -" ++ y ++ ")") xs ++ combineDvars xs
        names = map (("dComb" ++) . show) [1 .. (length $ combineDvars dVars)]
    buildFdu :: [String]
    buildFdu =
      (zipWith4 (\u d c n -> n ++ " = or(-" ++ u ++ ", -" ++ d ++ ", " ++ (intercalate ", " (map getLiteral c)) ++ ")") uVars dVars cnfClauses names)
        ++ ["Fdu = and(" ++ intercalate ", " names ++ ")"]
      where
        names = map (("fdu" ++) . show) clauseNums
        getLiteral :: Int -> String
        getLiteral ind = (if ind < 0 then "-" else "") ++ (vpVars !! (abs ind - 1))