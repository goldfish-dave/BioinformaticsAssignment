module Main
where

import MedianSearch
import DNA
import Distances
import MotifTrees
import Data.Foldable
import Prelude hiding (foldr)

file = "Data/mine-test.txt"

main = do
	fileLines <- fmap lines $ readFile file
	let dna = readDNA fileLines
	print $ simpMedSearch dna 8

res = do
	fileLines <- fmap lines $ readFile file
	return $ debugMedSearch (readDNA fileLines) 8

dna = do
	fileLines <- fmap lines $ readFile file 
	return $ readDNA fileLines
