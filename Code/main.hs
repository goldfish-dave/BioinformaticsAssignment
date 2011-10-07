module Main
where

import MedianSearch
import DNA
import MotifTrees
import Data.Foldable
import Prelude hiding (foldr)

main = do
	fileLines <- fmap lines $ readFile "Data/text-book-8-mer.txt"
	let dna = readDNA fileLines
	print $ simpMedSearch dna 8
