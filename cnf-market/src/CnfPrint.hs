module CnfPrint where

import ClassyPrelude
import Cnf
import qualified Data.Text as T

dimacsText :: Cnf -> Text
dimacsText (cnf@Cnf {cnfClauses}) =
  dimacsComment cnf ++ dimacsHeader cnf ++ concatMap dimacsTextClause cnfClauses

dimacsComment :: Cnf -> Text
dimacsComment Cnf {cnfComment} = unlines $ map ("c " ++) cnfComment

dimacsHeader :: Cnf -> Text
dimacsHeader Cnf {cnfClauses, cnfNumVars, cnfNumClauses} =
  "p cnf " ++ tshow cnfNumVars ++ " " ++ tshow cnfNumClauses ++ "\n"

dimacsTextClause :: Clause -> Text
dimacsTextClause clause = (unwords . map tshow $ clause) ++ " 0\n"

readCnf :: Text -> Cnf
readCnf input = foldr parseLine initialCnf contents
  where
    parseLine l cnf
      | Just rest <- stripPrefix "c" l =
        cnf {cnfComment = T.strip rest : cnfComment cnf}
      | Just rest <- stripPrefix "p" l =
        let Just [vars, clauses] = sequence $ map readMay $ drop 1 $ words rest
         in cnf {cnfNumVars = vars, cnfNumClauses = clauses}
      | otherwise =
        let Just clause = sequence $ initEx $ map readMay $ words l
         in cnf {cnfClauses = clause : cnfClauses cnf}

    initialCnf = mempty
    contents = lines input
