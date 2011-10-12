module MotifTrees
where

import Data.Tree
import Data.List (delete)

import Data.IORef
import Data.Foldable(minimumBy)

import Control.Concurrent
import Control.Concurrent.STM

type Score = Int
type Best a = (Score, a)
---------------------------------------------------------------------
compareBest (score,_) (score',_) = compare score score'
selectBest b b' = minimumBy compareBest [b,b']
-- Traversals

-- Simple Traverse traverses the search space like a tree without and branch or bounding
simpleTraverse :: (a -> Score) -> Tree a -> Best a -> Best a
simpleTraverse getScore (Node x []) (score,best)
	| score' < score = (score', x)
	| otherwise = (score,best)
	where score' = getScore x

simpleTraverse getScore (Node _ xs) best = minimumBy compareBest $ map (\tree -> simpleTraverse getScore tree best) xs

-- bnb Traverse uses branch and bound techniques to get a speed up on the traverse
bnbTraverse :: (a -> Score) -> Tree a -> Best a -> Best a
bnbTraverse getScore (score,best) (Node x []) 
	| score' < score = (score', x)
	| otherwise      = (score, best)
	where score'  = getScore x

bnbTraverse getScore b@(score,_) (Node x xs) 
	| score' < score = foldr (bnbTraverse getScore) b xs
	| otherwise      = b
	where score' = getScore x


-- cncrt Traverse uses concurrency (forkIO and IORef) to run the traverse in concurrent threads
-- Since IORefs do not have locks it's entirely possibly for things to go wrong
wrapper :: (Motif -> Int) -> Tree Motif  -> IO Motif
wrapper td tree = do
	--forks <- newMVar 0
	forks <- newMVar ([],0)
	best <- newIORef ([],100)
	maybeFork forks $ cncrtTraverse td tree forks best
	waitUntil ((== 0) . snd) forks
	(motif,score) <- readIORef best
	return motif

cncrtTraverse :: (Motif -> Int) -> Tree Motif -> MVar ForkRegister -> IORef BestWord -> IO ()
-- currently this doesn't return a value, it just modifies a value
cncrtTraverse totalDistance (Node x []) _ best = do
	let score' = totalDistance x
	(motif, score) <- readIORef best
	if score' < score
		then writeIORef best (x    , score') 
		else return ()

cncrtTraverse totalDistance (Node x xs) forkCount best = do
	let score' = totalDistance x
	(motif, score) <- readIORef best
	if score' < score
		then do
			mapM_ (maybeFork forkCount . \n -> cncrtTraverse totalDistance  n forkCount best) xs
		else return ()

--wrapper' :: (Motif -> Int) -> Tree Motif -> IO Motif
wrapper' :: (Motif -> Int) -> Tree Motif -> IO BestWord
wrapper' td tree = do
	forks <- atomically $ newTVar ([],0)
	best <- atomically $ newTVar ([],100)
	stmMaybeFork forks $ stmTraverse td tree forks best
	waitUntil' ((== 0) . snd) forks
	bw@(motif,_) <- atomically $ readTVar best
	return bw

stmTraverse :: (Motif -> Int) -> Tree Motif -> TVar ForkRegister -> TVar BestWord -> IO ()
stmTraverse totalDistance (Node x []) _ best = do
	let score' = totalDistance x
	atomically $ do
	(_, score) <- readTVar best
	if score' < score
		then writeTVar best (x , score')
		else return ()

stmTraverse totalDistance (Node x xs) forkCount best = do
	let score' = totalDistance x
	(_,score) <- atomically $ readTVar best
	if score' < score
		then mapM_ (stmMaybeFork forkCount . \n -> stmTraverse totalDistance n forkCount best) xs
		else return ()


---------------------------------------------------
-- Forking stuff

-- Don't make this less than 1
forkCap = 10 :: Int

type ForkRegister = ([ThreadId],Int)
-- type ForkRegister = Int

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

stmAddToReg :: TVar ([ThreadId],Int) -> IO ()
stmAddToReg tvar = do
	thisId <- myThreadId
	atomically $ do
		(ids, forks) <- readTVar tvar
		writeTVar tvar (thisId:ids, forks+1)

stmPopFromReg :: TVar ([ThreadId],Int) -> IO ()
stmPopFromReg tvar = do
	thisId <- myThreadId
	atomically $ do
		(ids,forks) <- readTVar tvar
		writeTVar tvar (delete thisId ids, forks-1)

maybeFork :: MVar ([ThreadId],Int) -> IO () -> IO ()
-- will run the io action in a new fork unless
-- the number of forks represented in cap is 
-- greater than forkCap
maybeFork reg io = do
	(_,forksCount) <- readMVar reg
	if forksCount < forkCap
		then addToRegister reg >> (forkIO $ io >> popFromRegister reg) >> return ()
		else io

stmMaybeFork :: TVar ForkRegister -> IO () -> IO ()
stmMaybeFork reg io = do
	(_,forksCount) <- atomically $ readTVar reg
	if forksCount < forkCap
		then stmAddToReg reg >> (forkIO $ io >> stmPopFromReg reg) >> return ()
		else io

waitUntil :: (a -> Bool) -> MVar a -> IO ()
waitUntil pred ref = do
	x <- readMVar ref
	if pred x
		then return ()
		else threadDelay 100000 >> waitUntil pred ref

waitUntil' :: (a -> Bool) -> TVar a -> IO ()
waitUntil' pred tvar = do
	x <- atomically $ readTVar tvar
	if pred x
		then return ()
		else threadDelay 100000 >> waitUntil' pred tvar


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
