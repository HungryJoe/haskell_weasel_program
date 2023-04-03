module Main where

import Ace (AceWeasel (..))
import qualified Util (spawnInitialGeneration, evolve)
import Allo (AlloWeasel (..))
import System.Environment (getArgs)
import System.Random (initStdGen)

-- main <target> [mut-chance] [max-iterations]
main :: IO ()
main = do
  args <- getArgs
  let target = head args
  let mutChance = if length args == 3 then read (args !! 1) else 0.05
  let numCopies = if length args == 3 then read (args !! 2) else 100
  rng <- initStdGen
  let aceWeasel = Ace.AceWeasel {Ace.popSize=numCopies, Ace.mutChance=mutChance}
  let aceInitialGen = Util.spawnInitialGeneration (length target) aceWeasel rng
  let (aceGenerations, rng') = Util.evolve target aceWeasel aceInitialGen

  let alloWeasel = AlloWeasel {Allo.popSize=numCopies, Allo.mutChance=mutChance}
  let alloInitialGen = Util.spawnInitialGeneration (length target) alloWeasel rng
  let (alloGenerations, rng') = Util.evolve target alloWeasel alloInitialGen

  print $ "It took " ++ show (length aceGenerations) ++ " to asexually generate:"
  print $ head $ last aceGenerations
  print $ "It took " ++ show (length alloGenerations) ++ " to allosexually generate:"
  print $ last alloGenerations
