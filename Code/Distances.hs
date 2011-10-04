module Distances
where

import DNA

totDist :: DNA -> Int -> [NukeTide] -> Int
totDist dna l v = minimum $ map (\line -> minimum [ ham v s | s <- possibles line ]) dna
	where
		possibles string = [ take l $ drop i string | i <- [0 .. length string] ]

ham :: [NukeTide] -> [NukeTide] -> Int
ham [] [] = 0
ham xs [] = length xs
ham [] ys = length ys
ham (x:xs) (y:ys) = case (x == y) of
	True  -> ham xs ys
	False -> 1 + ham xs ys

