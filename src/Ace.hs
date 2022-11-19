module Ace (spawnInitialGeneration, evolve) where
import System.Random
import Data.Bifunctor (first)
import Data.List (maximumBy)
import Util (mutate, calculateFitness, genLetter)

copyAndMutate :: RandomGen g => Float -> Int -> (String, g) -> ([String], g)
copyAndMutate _ 0 (_, randomGen) = ([], randomGen)
copyAndMutate probability numCopies (initialCopy, randomGen) = first (copy:) $ copyAndMutate probability (numCopies - 1) (initialCopy, randomGen')
    where (copy, randomGen') = mutate probability (initialCopy, randomGen)

spawnNextGeneration :: RandomGen g => String -> Float -> Int -> (String, g) -> (String, g)
spawnNextGeneration target mutChance numCopies current = first (snd . findMostFit) (copyAndMutate mutChance numCopies current)
    where appendFitness copy = (calculateFitness target copy, copy)
          findMostFit copies = maximumBy compareByRank $ map appendFitness copies
          compareByRank (rankA, _) (rankB, _) = compare rankA rankB

evolve :: RandomGen g => String -> Float -> Int -> (String, g) -> ([String], g)
evolve target mutChance numCopies current
    | calculateFitness target nextGen == length target = ([nextGen], randomGen)
    | otherwise = first (nextGen:) $ evolve target mutChance numCopies next
    where next@(nextGen, randomGen) = spawnNextGeneration target mutChance numCopies current

spawnInitialGeneration :: RandomGen g => g -> Int -> (String, g)
spawnInitialGeneration randomGen 0 = ("", randomGen)
spawnInitialGeneration randomGen len = first (nextLetter:) $ spawnInitialGeneration randomGen' (len - 1)
    where (nextLetter, randomGen') = genLetter randomGen
