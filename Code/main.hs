module Main
where

import MedianSearch
import DNA
import Distances
import MotifTrees
import Data.Foldable
import Prelude hiding (foldr)

import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM

--file = "Data/mine-test.txt"
file = "Data/text-book-8-mer.txt"

main = do
--	print $ bnbMedSearch dna 8
--	cncrtTraverse
--	(count,best) <-test
--	readLoop 10 count
	fileLines <- fmap lines $ readFile file
	let	dna = readDNA fileLines
		l = 8
		td = scoreFunction dna l
		tree = searchTree l
		threads = 4
	print $ boundingMedianSearch dna l
	{-
	simpleMedianSearch dna l
	boundingMedianSearch dna l
	lockingMedianSearch threads dna l
	stmMedianSearch threads dna l
	-}
	return ()

	

readLoop :: Show a => Int -> IORef a -> IO ()
readLoop 0 _ = return ()
readLoop n ioref = do
	threadDelay 300000
	printIORef ioref
	readLoop (n-1) ioref

{-
test :: IO (MVar Int, IORef BestWord)
test = do
	fileLines <- fmap lines $ readFile file
	let	dna = readDNA fileLines
		td = scoreFunction dna 8
		tree = searchTree 8
	best <- newIORef ([],1000)
	--forksCount <- newMVar 0
	forksCount <- newMVar ([],0)
	forkIO $ cncrtTraverse td tree forksCount best
	return (forksCount,best)
-}

printIORef :: Show a => IORef a -> IO ()
printIORef ref = do
	val <- readIORef ref
	print val

dna = do
	fileLines <- fmap lines $ readFile file 
	return $ readDNA fileLines
