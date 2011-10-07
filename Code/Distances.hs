module Distances
where

import DNA
import MotifTrees

{-
hammingDistance :: Motif -> Motif -> Int
hammingDistance [] [] = 0
hammingDistance xs [] = length xs
hammingDistance [] ys = length ys
hammingDistance (x:xs) (y:ys) = case (x == y) of
	True  -> hammingDistance xs ys
	False -> 1 + hammingDistance xs ys
-}
hammingDistance :: Motif -> Motif -> Int
hammingDistance [] [] = 0
hammingDistance (x:xs) (y:ys) = case (x == y) of
	True  -> hammingDistance xs ys
	False -> 1 + hammingDistance xs ys
hammingDistance _ _ = error "hammingDistance: applying on arguments of non equal lengths."

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
		-- TODO: comment/clarify this
		totalDistance motif = sum $  map (minimum . map (hammingDistance motif) . motifs l) dna

		motifs :: Int -> [NukeTide] -> [Motif]
		motifs l nukeTides = [ take l $ drop n nukeTides | n <- [0..length nukeTides - l] ]

