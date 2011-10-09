module MotifTrees
where

import DNA
import Distances
import Data.Tree

import Data.List (delete)
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

type ForkRegister = ([ThreadId],Int)
-- type ForkRegister = Int
-- cncrt Traverse uses concurrency (forkIO and IORef) to run the traverse in concurrent threads
-- Since IORefs do not have locks it's entirely possibly for things to go wrong
wrapper :: (Motif -> Int) -> Tree Motif  -> IO Motif
wrapper td tree = do
	--forks <- newMVar 0
	forks <- newMVar ([],0)
	best <- newIORef ([],100)
	maybeFork' forks $ cncrtSimpleTraverse td tree forks best
	waitUntil ((== 0) . snd) forks
	(motif,score) <- readIORef best
	return motif

cncrtSimpleTraverse :: (Motif -> Int) -> Tree Motif -> MVar ForkRegister -> IORef BestWord -> IO ()
-- currently this doesn't return a value, it just modifies a value
cncrtSimpleTraverse totalDistance (Node x []) _ best = do
	(motif, score) <- readIORef best
	let score' = totalDistance x
	if score' < score
	then writeIORef best (x    , score') 
	else return ()

cncrtSimpleTraverse totalDistance (Node x xs) forkCount best = do
	(motif, score) <- readIORef best
	let score' = totalDistance x
	if score' < score
	then do
		writeIORef best (motif, score)
		mapM_ (maybeFork' forkCount . \n -> cncrtSimpleTraverse totalDistance  n forkCount best) xs
	else return ()



---------------------------------------------------
-- Forking stuff

-- Don't make this less than 1
forkCap = 4 :: Int

incrementCap :: MVar Int -> IO ()
incrementCap cap = takeMVar cap >>= putMVar cap . (+1) >> return ()

decrementCap :: MVar Int -> IO ()
decrementCap cap = takeMVar cap >>= putMVar cap . ((-) 1) >> return ()

addToRegister :: MVar ([ThreadId],Int) -> IO ()
addToRegister mvar = do
	(ids,forks) <- takeMVar mvar
	thisId <- myThreadId
	putMVar mvar (thisId:ids, forks+1)

popFromRegister :: MVar ([ThreadId],Int) -> IO ()
popFromRegister mvar = do
	(ids,forks) <- takeMVar mvar
	thisId <- myThreadId
	putMVar mvar (delete thisId ids, forks-1)

maybeFork :: MVar Int -> IO () -> IO ()
-- will run the io action in a new fork unless
-- the number of forks represented in cap is 
-- greater than forkCap
maybeFork cap io = do
	forksCount <- readMVar cap
	if forksCount < forkCap
	then incrementCap cap >> (forkIO $ io >> decrementCap cap) >> return ()
	else io

maybeFork' :: MVar ([ThreadId],Int) -> IO () -> IO ()
-- will run the io action in a new fork unless
-- the number of forks represented in cap is 
-- greater than forkCap
maybeFork' reg io = do
	(_,forksCount) <- readMVar reg
	if forksCount < forkCap
	then addToRegister reg >> (forkIO $ io >> popFromRegister reg) >> return ()
	else io

waitUntil :: (a -> Bool) -> MVar a -> IO a
waitUntil pred ref = do
	x <- takeMVar ref
	if pred x
	then putMVar ref x >> return x
	else putMVar ref x >> threadDelay 100000 >> waitUntil pred ref

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
