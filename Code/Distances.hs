module Distances
where

import DNA

hammingDistance :: Motif -> Motif -> Int
hammingDistance [] [] = 0
hammingDistance xs [] = length xs
hammingDistance [] ys = length ys
hammingDistance (x:xs) (y:ys) = case (x == y) of
	True  -> hammingDistance xs ys
	False -> 1 + hammingDistance xs ys

bestOf :: BestWord -> BestWord -> BestWord
bestOf bw@(motif, score) bw'@(motif', score')
	| score > score' = bw
	| otherwise      = bw'

scoreFunction :: DNA -> Int -> (Motif -> Int)
scoreFunction dna l = totalDistance
	where
		-- TODO: comment/clarify this
		totalDistance motif = minimum $  map (minimum . map (hammingDistance motif) . motifs l) dna

		motifs :: Int -> [NukeTide] -> [Motif]
		motifs l nukeTids = [ take l $ drop n nukeTids | n <- [0..length nukeTides - l] ]

