module Main
where

import MedianSearch
import DNA
import Distances
import MotifTrees
import Data.Foldable
import Prelude hiding (foldr)
import Control.Concurrent

file = "Data/mine-test.txt"
--file = "Data/text-book-8-mer.txt"

main = do
--	print $ bnbMedSearch dna 8
--	cncrtSimpleTraverse
	return ()

test :: IO (MVar Int, MVar BestWord)
test = do
	fileLines <- fmap lines $ readFile file
	let	dna = readDNA fileLines
		td = scoreFunction dna 8
		tree = searchTree 8
	best <- newMVar ([],1000)
	forksCount <- newMVar 0
	forkIO $ cncrtSimpleTraverse td tree forksCount best
	return (forksCount,best)

read :: Show a => MVar a -> IO ()
read mvar = do
	val <- takeMVar mvar
	print val
	putMVar mvar val

dna = do
	fileLines <- fmap lines $ readFile file 
	return $ readDNA fileLines
