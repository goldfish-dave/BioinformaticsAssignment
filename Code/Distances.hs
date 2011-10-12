module Distances
where

import DNA
import Data.List(tails)

infinity' = maxBound :: Int

hammingDistance :: Motif -> Motif -> Int
hammingDistance xs ys = go 0 xs ys
	where
		go !acc [] [] = acc
		go !acc xs [] = acc
		go !acc [] ys = acc
		go !acc (x:xs) (y:ys) = case (x == y) of
			True -> go acc xs ys
			False -> go (acc+1) xs ys

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

