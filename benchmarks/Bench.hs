import Control.Monad (forM)
import Criterion.Main
import RandomSat
import System.Random
import UnsatCore

-- Our benchmark harness.
main :: IO ()
main = do
  let stdGen = mkStdGen 46
  let gens = randomGens stdGen
  let cnfs clauseLen variables clauseNum gen =
        let randomCnfConfig =
              RandomCnfConfig
                { randomConfigClauseLength = clauseLen,
                  randomConfigNumVars = variables,
                  randomConfigNumClauses = clauseNum,
                  randomConfigComment = []
                }
         in randomCnf randomCnfConfig gen
  let configs =
        map (\(cnfConfig, gen) -> (cnfConfig, uncurry3 cnfs cnfConfig gen)) $
          zip
            [ (3, 10, 40),
              (3, 100, 400),
              (4, 100, 400),
              (3, 500, 2000),
              (4, 500, 2000)
            ]
            gens

  benchmarkGroups <- forM configs $ \((clauseLen, variables, clauseNum), cnf) -> do
    pure $
      bgroup
        ("Random Cnf " ++ show (clauseLen, variables, clauseNum))
        [ bench "sat" $ whnfIO (findUnsatCore cnf)
        ]

  defaultMain benchmarkGroups

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (a, b, c) = f a b c
