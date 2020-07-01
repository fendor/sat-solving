module Main where

import ClassyPrelude
import Cnf
import CnfPrint
import Options.Applicative
import RandomSat
import System.Random
import Text.Read
import Ptn

data CnfOption
  = Rand { optionClauseLength :: Int
         , optionNumVar :: Int
         , optionNumClauses :: Int
         , optionSeed :: Maybe Int }
  | RandSat { optionClauseLength :: Int
            , optionNumVar :: Int
            , optionNumClauses :: Int
            , optionAssignment :: Maybe [Int]
            , optionSeed :: Maybe Int }
  | Ptn { optionNumVar :: Int }
  deriving (Show)

main :: IO ()
main = do
  options <- execParser $ info (parser <**> helper) fullDesc
  putStr "c "
  print options
  cnf <- genCnf options
  putStr . dimacsText $ cnf

parser :: Parser CnfOption
parser =
  hsubparser $
  command "rand" (info randOptions fullDesc) ++
  command "rand-sat" (info randSatOptions fullDesc) ++
  command "ptn" (info ptnOptions fullDesc)


randOptions :: Parser CnfOption
randOptions =
  Rand <$> option auto (short 'k') <*> option auto (short 'n') <*>
  option auto (short 'm') <*>
  optional (option auto (long "seed"))

randSatOptions :: Parser CnfOption
randSatOptions =
  RandSat <$> option auto (short 'k') <*> option auto (short 'n') <*>
  option auto (short 'm') <*>
  optional (option auto (long "assignment")) <*>
  optional (option auto (long "seed"))

ptnOptions :: Parser CnfOption
ptnOptions = Ptn <$> option auto (short 'n')

genCnf :: CnfOption -> IO Cnf
genCnf Rand {..} = do
  defaultSeed <- randomIO
  let seed = fromMaybe defaultSeed optionSeed
  let gen = mkStdGen seed
  let randomConfig =
        RandomCnfConfig
          optionClauseLength
          optionNumVar
          optionNumClauses
          ["seed: " ++ tshow seed]
  return $ randomCnf randomConfig gen
genCnf RandSat {..} = do
  defaultSeed <- randomIO
  let seed = fromMaybe defaultSeed optionSeed
  let gen' = mkStdGen seed
  let (gen, assignmentGen) = split gen'
  let randConfig =
        RandomCnfConfig
          optionClauseLength
          optionNumVar
          optionNumClauses
          ["seed: " ++ tshow seed]
  let assignment =
        fromMaybe (randomAssignment randConfig assignmentGen) optionAssignment
  return $ randomSatCnf assignment randConfig gen
genCnf Ptn{..} =
  return $ ptnCnf optionNumVar
