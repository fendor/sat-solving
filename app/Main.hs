module Main where

import RandomSat
import System.Random
import qualified QBFUnsatCore as Qbf
import qualified UnsatCore as Sat
import qualified Muser

main :: IO ()
main = do
  let stdGen = mkStdGen 42
  let randomCnfConfig = RandomCnfConfig 2 3 9 []
  let cnf = randomCnf randomCnfConfig stdGen

  -- T.putStrLn $ dimacsText cnf
  print =<< Sat.findUnsatCore cnf
  print =<< Qbf.findUnsatCore cnf
  print =<< Muser.findUnsatCore cnf
  -- pure ()
