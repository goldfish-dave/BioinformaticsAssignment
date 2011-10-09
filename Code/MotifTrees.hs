module MotifTrees
where

import DNA
import Distances
import Data.Tree

import Control.Concurrent

---------------------------------------------------------------------
-- Traversals

simpleTraverse :: Tree a -> [a]
simpleTraverse (Node x []) = [x]
simpleTraverse (Node _ xs) = concatMap simpleTraverse xs


bnbTraverse :: (Motif -> Int) -> Tree Motif -> BestWord -> BestWord
bnbTraverse totalDistance (Node x []) (motif, score)
	| score' < score = (x    , score')
	| otherwise      = (motif, score )
	where score'  = totalDistance x
bnbTraverse totalDistance (Node x xs) (motif, score)
	| score' < score = foldr (bnbTraverse totalDistance) (motif, score) xs
	| otherwise      = (motif, score)
	where score' = totalDistance x

cncrtSimpleTraverse :: (Motif -> Int) -> Tree Motif -> MVar Int -> MVar BestWord -> IO ()
-- currently this doesn't return a value, it just modifies a value
cncrtSimpleTraverse totalDistance (Node x []) _ best = do
	(motif, score) <- takeMVar best
	let score' = totalDistance x
	if score' < score
	then putMVar best (x    , score') >> (putStr $ "New best: " ++ show (x,score'))
	else putMVar best (motif,  score)

cncrtSimpleTraverse totalDistance (Node x xs) forkCount best = do
	(motif, score) <- takeMVar best
	let score' = totalDistance x
	if score' < score
	then do
		putMVar best (motif, score)
		--print score
		mapM_ (maybeFork forkCount . \n -> cncrtSimpleTraverse totalDistance  n forkCount best) xs
	else return ()

forkCap = 0 :: Int

incrementCap :: MVar Int -> IO ()
incrementCap cap = takeMVar cap >>= putMVar cap . (+1)

decrementCap :: MVar Int -> IO ()
decrementCap cap = takeMVar cap >>= putMVar cap . ((-) 1)

maybeFork :: MVar Int -> IO () -> IO ()
-- will run the io action in a new fork unless
-- the number of forks represented in cap is 
-- greater than forkCap
maybeFork cap io = do
	forksCount <- takeMVar cap
	if forksCount < forkCap
	then incrementCap cap >> (forkIO $ io >> decrementCap cap) >> return ()
	else io


-----------------------------------------------------------------------
-- Tree generation

searchTree :: Int -> Tree Motif
-- This function takes a motif length l and returns a
-- search tree containing all possible motifs of length l
-- as its leaf nodes.
searchTree l
	| l < 0 = error "searchTree: must use positive l!"
	| otherwise = unfoldTree (branchTree) (seed)
	where
		branchTree :: BranchShoot -> (Motif, [BranchShoot])
		branchTree (0, motif) = ( motif, [] )
		branchTree (n, motif) = 
			let
				newMotifs = map (\base -> motif ++ [base]) [A,T,C,G]
				nextLevel = repeat (n-1)
				branchShoots = nextLevel `zip` newMotifs
			in	(motif, branchShoots)
		seed = (l, []) -- [] represents a motif of length 0

testTree = Node 5 [
			Node 3 [
			 Node 1 [],
			 Node 6 []],
			Node 9 [
			 Node 8 [],
			 Node 10 [Node 0 []]]]
