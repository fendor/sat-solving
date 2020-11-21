module Main where

import Cnf
import CnfPrint
import Control.Monad
import Control.StopWatch
import Data.List
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Muser
import qualified QBFUnsatCore as Qbf
import System.Clock
import System.Directory
import System.Environment (getArgs)
import qualified UnsatCore as Sat
import System.IO

main :: IO ()
main = do
  files <- getArgs
  let l = length files
  forM_ (zip [1..] files) $ \(i, file) -> do
    hPutStrLn stderr $ "File " ++ show i ++ "/" ++ show l
    hPutStrLn stderr $ "Process input: " ++ file
    (_, t) <- stopWatch $ do
      cnf <- readCnf <$> T.readFile file

      -- T.putStrLn $ dimacsText cnf
      (unsatCoreNaive, timeNaive) <- stopWatch (Sat.findUnsatCore cnf)
      (unsatCoreQbf, timeQbf) <- stopWatch (Qbf.findUnsatCore cnf)
      (unsatCoreMuser, timeMuser) <- stopWatch (Muser.findUnsatCore cnf)

      let benchFp = "benchmark.csv"
      exists <- doesFileExist benchFp
      unless exists $ do
        T.writeFile benchFp "input,type,timeNs,size,sat\n"

      T.appendFile benchFp $ csvLine file "naive" unsatCoreNaive timeNaive
      T.appendFile benchFp $ csvLine file "qbf" unsatCoreQbf timeQbf
      T.appendFile benchFp $ csvLine file "muser" unsatCoreMuser timeMuser

    hPutStrLn stderr $ "Processed in " ++ show (fromInteger (toNanoSecs t) / 1e9) ++ "s"
  where
    csvLine :: FilePath -> String -> Maybe (Seq.Seq Clause) -> TimeSpec -> T.Text
    csvLine input solverType solution timeSpec =
      T.pack
        ( intercalate
            ","
            [ input,
              solverType,
              show $ toNanoSecs timeSpec,
              show $ maybe 0 Seq.length solution,
              maybe "yes" (const "no") solution
            ]
        )
        <> "\n"

-- pure ()
