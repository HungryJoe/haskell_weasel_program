module Weasel (spawnInitialGeneration, evolve) where
import System.Random
import Data.Bifunctor (first)
import Data.List (maximumBy)

copyAndMutate :: RandomGen g => Float -> Int -> (String, g) -> ([String], g)
copyAndMutate _ 0 (_, randomGen) = ([], randomGen)
copyAndMutate probability numCopies (initialCopy, randomGen) = first (copy:) $ copyAndMutate probability (numCopies - 1) (initialCopy, randomGen')
    where (copy, randomGen') = mutate probability (initialCopy, randomGen)

mutate :: RandomGen g => Float -> (String, g) -> (String, g)
-- Generate a string identical to organism except each letter has a probability * 100% chance to be changed to a random other letter
mutate probability (a:rest, randomGen) = first (a':) $ mutate probability (rest, randomGen')
    where (a', randomGen') = mutateChar probability (a, randomGen)
mutate _ ([], randomGen) = ("", randomGen)

mutateChar :: RandomGen g => Float -> (Char, g) -> (Char, g)
mutateChar probability (ch, randomGen)
    | doMutate = genLetter randomGen 
    | otherwise = (ch, randomGen')
    where (doMutate, randomGen') = (fst (roll randomGen) < probability, snd (roll randomGen))
          roll = uniformR (0 :: Float, 1 :: Float)

-- Fitness is in [0, min (length left) (length right)]
calculateFitness :: String -> String -> Int
calculateFitness left right = sum $ zipWith score left right
    where score x y = if x == y then 1 else 0

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

genLetter :: RandomGen g => g -> (Char, g)
genLetter randomGen = first intToChar (uniformR (0, 26) randomGen)
    where intToChar int
              | int < 26 = ['a'..'z'] !! int
              | otherwise = ' '

spawnInitialGeneration :: RandomGen g => g -> Int -> (String, g)
spawnInitialGeneration randomGen 0 = ("", randomGen)
spawnInitialGeneration randomGen len = first (nextLetter:) $ spawnInitialGeneration randomGen' (len - 1)
    where (nextLetter, randomGen') = genLetter randomGen
