module Main where

import Ace (evolve, spawnInitialGeneration)
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
  let initialGen = spawnInitialGeneration rng (length target)
  let (generations, rng') = evolve target mutChance numCopies initialGen
  print $ "It took " ++ show (length generations) ++ " to generate:"
  print $ last generations
