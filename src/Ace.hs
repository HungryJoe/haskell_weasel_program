module Ace (spawnInitialGeneration, evolve, copyAndMutate) where
import System.Random
import Data.Bifunctor (first)
import Data.List (maximumBy)
import Util (mutate, calculateFitness, genLetter, spawnOrganism, findMostFit)

copyAndMutate :: RandomGen g => Float -> Int -> (String, g) -> ([String], g)
copyAndMutate _ 0 (_, randomGen) = ([], randomGen)
copyAndMutate probability numCopies (initialCopy, randomGen) = first (copy:) $ copyAndMutate probability (numCopies - 1) (initialCopy, randomGen')
    where (copy, randomGen') = mutate probability (initialCopy, randomGen)

spawnNextGeneration :: RandomGen g => String -> Float -> Int -> (String, g) -> (String, g)
spawnNextGeneration target mutChance numCopies current = first (head . findMostFit target 1) (copyAndMutate mutChance numCopies current)

evolve :: RandomGen g => String -> Float -> Int -> (String, g) -> ([String], g)
evolve target mutChance numCopies current
    | calculateFitness target nextGen == length target = ([nextGen], randomGen)
    | otherwise = first (nextGen:) $ evolve target mutChance numCopies next
    where next@(nextGen, randomGen) = spawnNextGeneration target mutChance numCopies current

spawnInitialGeneration :: RandomGen g => g -> Int -> (String, g)
spawnInitialGeneration = spawnOrganism
