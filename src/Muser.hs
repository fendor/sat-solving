module Muser where

import Cnf
import CnfPrint
import Control.Exception
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as T
import System.Exit
import System.Process

findUnsatCore :: Cnf -> IO (Maybe (Seq Clause))
findUnsatCore cnf = do
  T.writeFile "muser.cnf" (dimacsText cnf)
  invokeMuser "muser.cnf"
    >>= \case
      Nothing -> pure Nothing
      Just resFp -> do
        cnfContent <- T.readFile resFp
        pure . Just . Seq.fromList . cnfClauses $ readCnf cnfContent

invokeMuser :: FilePath -> IO (Maybe FilePath)
invokeMuser fp = do
  (res, _serr, _sout) <- readProcessWithExitCode "muser2" ["-w", fp] ""
  case res of
    ExitSuccess -> pure Nothing
    ExitFailure 20 -> pure $ Just "muser2-output.cnf"
    ExitFailure n -> throwIO $ userError $ "Invariant broken, non standard exit-code: " ++ show n