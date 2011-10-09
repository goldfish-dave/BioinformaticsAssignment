module MotifTrees
where

import DNA
import Distances
import Data.Tree

import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM

---------------------------------------------------------------------
-- Traversals

-- Simple Traverse traverses the search space like a tree without and branch or bounding
simpleTraverse :: Tree a -> [a]
simpleTraverse (Node x []) = [x]
simpleTraverse (Node _ xs) = concatMap simpleTraverse xs

-- bnb Traverse uses branch and bound techniques to get a speed up on the traverse
bnbTraverse :: (Motif -> Int) -> Tree Motif -> BestWord -> BestWord
bnbTraverse totalDistance (Node x []) (motif, score)
	| score' < score = (x    , score')
	| otherwise      = (motif, score )
	where score'  = totalDistance x
bnbTraverse totalDistance (Node x xs) (motif, score)
	| score' < score = foldr (bnbTraverse totalDistance) (motif, score) xs
	| otherwise      = (motif, score)
	where score' = totalDistance x

-- cncrt Traverse uses concurrency (forkIO and IORef) to run the traverse in concurrent threads
-- Since IORefs do not have locks it's entirely possibly for things to go wrong
cncrtSimpleTraverse :: (Motif -> Int) -> Tree Motif -> IORef Int -> IORef BestWord -> IO ()
-- currently this doesn't return a value, it just modifies a value
cncrtSimpleTraverse totalDistance (Node x []) _ best = do
	(motif, score) <- readIORef best
	let score' = totalDistance x
	if score' < score
	then writeIORef best (x    , score') >> (putStr $ "New best: " ++ show (x,score'))
	else return ()

cncrtSimpleTraverse totalDistance (Node x xs) forkCount best = do
	(motif, score) <- readIORef best
	let score' = totalDistance x
	if score' < score
	then do
		writeIORef best (motif, score)
		--print score
		mapM_ (maybeFork forkCount . \n -> cncrtSimpleTraverse totalDistance  n forkCount best) xs
	else return ()

---------------------------------------------------
-- Forking stuff

forkCap = 0 :: Int

incrementCap :: IORef Int -> IO ()
incrementCap cap = readIORef cap >>= writeIORef cap . (+1)

decrementCap :: IORef Int -> IO ()
decrementCap cap = readIORef cap >>= writeIORef cap . ((-) 1)

maybeFork :: IORef Int -> IO () -> IO ()
-- will run the io action in a new fork unless
-- the number of forks represented in cap is 
-- greater than forkCap
maybeFork cap io = do
	forksCount <- readIORef cap
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
