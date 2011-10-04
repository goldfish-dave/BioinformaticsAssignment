module Distances
where

totDist :: DNA -> [NukeTide] -> Int
totDist dna v = min . map (hamDist v) $ dna

hamDist :: [NukeTide] -> DNA -> Int
hamDist v s' = sum . map (ham v) $ s'

ham :: [NukeTide] -> [NukeTide] -> Int
ham xs [] = length xs
ham [] ys = length ys
ham (x:xs) (y:ys) = case (x == y) of
	True  -> ham xs ys
	False -> 1 + ham xs ys
