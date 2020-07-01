module CnfPrint where

import ClassyPrelude
import Cnf

dimacsText :: Cnf -> Text
dimacsText (cnf@Cnf { cnfClauses }) =
  dimacsComment cnf ++ dimacsHeader cnf ++ concatMap dimacsTextClause cnfClauses

dimacsComment :: Cnf -> Text
dimacsComment Cnf { cnfComment } = unlines $ map ("c " ++) cnfComment

dimacsHeader :: Cnf -> Text
dimacsHeader Cnf { cnfClauses, cnfNumVars, cnfNumClauses } =
  "p cnf " ++ tshow cnfNumVars ++ " " ++ tshow cnfNumClauses ++ "\n"

dimacsTextClause :: Clause -> Text
dimacsTextClause clause = (unwords . map tshow $ clause) ++ " 0\n"
