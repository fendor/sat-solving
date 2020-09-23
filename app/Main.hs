module Main where

import RandomSat
import System.Random
import QBFUnsatCore
import CnfPrint
import qualified Data.Text.IO as T

main :: IO ()
main = do
  let stdGen = mkStdGen 45
  let randomCnfConfig = RandomCnfConfig 3 4 5 []
  let cnf = randomCnf randomCnfConfig stdGen

  T.putStrLn $ dimacsText cnf
  putStrLn $ reduceToQCir cnf
  pure ()
