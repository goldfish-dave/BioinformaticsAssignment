module Main
where

import MedianSearch
import DNA

main = do
	fileLines <- fmap lines $ readFile "Data/text-book-8-mer-b.txt"
	let dna = readDNA fileLines
	print $ bfMedSerch dna 8
