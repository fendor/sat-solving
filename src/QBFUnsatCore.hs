{-# LANGUAGE RecordWildCards #-}
module QBFUnsatCore where


import Cnf
import Data.List (intercalate)

reduceToQCir :: Cnf -> String
reduceToQCir Cnf {..} =
  unlines
    [ "#QCIR-G14",
      "exists(" ++ intercalate "," uVars ++ ")",
      "forall(" ++ intercalate "," vVars ++ ")",
      "forall(" ++ intercalate "," dVars ++ ")",
      "exists(" ++ intercalate "," vpVars ++ ")",
      "output(Fo)",
      unlines buildQ1,
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
      (zipWith3 (\c u n -> n ++ "= or(-" ++ u ++ "," ++ intercalate "," (map show c) ++ ")") cnfClauses uVars clauseNames)
        ++ ["Q1 = and(" ++ intercalate "," clauseNames ++ ")"]
      where
        clauseNames = map (("q1Clause" ++) . show) clauseNums