module Util where
import System.Random (RandomGen, uniformR)
import Data.Bifunctor (Bifunctor(first))
import Data.List (maximumBy, sortBy, sortOn)
import Data.Function (on)
import Data.Ord

class EvolutionaryAlgorithm a where
    spawnInitialGeneration :: RandomGen g => Int -> a -> g -> ([String], g)
    spawnNextGeneration :: RandomGen g => String -> a -> ([String], g) -> ([String], g)

evolve :: (RandomGen g, EvolutionaryAlgorithm ea) => String -> ea -> ([String], g) -> ([[String]], g)
evolve target evAlg current@(currentGen, randomGen)
    | calculateFitness target (head $ findMostFit target 1 currentGen) == length target = ([currentGen], randomGen)
    | otherwise = first (nextGen:) $ evolve target evAlg next
    where next@(nextGen, _) = spawnNextGeneration target evAlg current

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

genLetter :: RandomGen g => g -> (Char, g)
genLetter randomGen = first intToChar (uniformR (0, 26) randomGen)
    where intToChar int
              | int < 26 = ['a'..'z'] !! int
              | otherwise = ' '

spawnOrganism :: RandomGen g => g -> Int -> (String, g)
spawnOrganism randomGen 0 = ("", randomGen)
spawnOrganism randomGen len = first (nextLetter:) $ spawnOrganism randomGen' (len - 1)
    where (nextLetter, randomGen') = genLetter randomGen


findMostFit :: String -> Int -> [String] -> [String]
findMostFit target numMostFit copies = map snd $ take numMostFit $ sortOn (Down . fst) $ map (appendFitness target) copies
    where appendFitness target organism = (calculateFitness target organism, organism)

mutateAll :: RandomGen g => Float -> ([String], g) -> ([String], g)
mutateAll mutChance ([], randomGen) = ([], randomGen)
mutateAll mutChance (org:rest, randomGen) = first (mutatedOrg:) $ mutateAll mutChance (rest, randomGen')
    where (mutatedOrg, randomGen') = mutate mutChance (org, randomGen)
