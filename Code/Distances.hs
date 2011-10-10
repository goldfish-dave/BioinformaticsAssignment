module Distances
where

import DNA
import Data.List(tails)

{-
hammingDistance :: Motif -> Motif -> Int
hammingDistance [] [] = 0
hammingDistance xs [] = 0 -- optimistic
hammingDistance [] ys = 0 -- optimistic
hammingDistance (x:xs) (y:ys) = case (x == y) of
	True  -> hammingDistance xs ys
	False -> 1 + hammingDistance xs ys
-}

hammingDistance :: Motif -> Motif -> Int
hammingDistance xs ys = length . filter (uncurry (==)) $ zip xs ys

bestOf :: BestWord -> BestWord -> BestWord
bestOf bw@(motif, score) bw'@(motif', score')
	| score < score' = bw
	| otherwise      = bw'

compareMotifs :: (Motif -> Int) -> Motif -> Motif -> Ordering
compareMotifs totalDistance motif motif' = compare score score'
	where
		(score, score') = (totalDistance motif, totalDistance motif')

scoreFunction :: DNA -> Int -> (Motif -> Int)
scoreFunction dna l = totalDistance
	where
		-- The sum of the minimum hamming distance in each line of dna
		-- is given by totalDistance motif
		totalDistance motif = sum $ map (minimum . map (hammingDistance motif)) possibleMotifs
		possibleMotifs = map (motifs l) dna

motifs :: Int -> [a] -> [[a]]
motifs n nukeTides = map (take n) $ take count $ tails nukeTides
	where count = length nukeTides - n + 1

