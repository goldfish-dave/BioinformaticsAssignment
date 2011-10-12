module MotifTrees
where

import Data.Tree
import Data.List (delete)

import Data.IORef
import Data.Foldable(minimumBy)

import Control.Concurrent
import Control.Concurrent.STM

import Forks

type Score = Int
type Best a = (Score, a)
---------------------------------------------------------------------
compareBest (score,_) (score',_) = compare score score'
selectBest b b' = minimumBy compareBest [b,b']
-- Traversals

-- Simple Traverse traverses the search space like a tree without and branch or bounding
simpleTraverse :: (a -> (Score, b)) -> Tree a -> Best (a,b) -> Best (a,b)
simpleTraverse getScore (Node x []) best@(score,_)
	| score' < score = (score', (x, positions))
	| otherwise = best
	where (score',positions) = getScore x

simpleTraverse getScore (Node _ xs) best = minimumBy compareBest $ map (\tree -> simpleTraverse getScore tree best) xs

-- bnb Traverse uses branch and bound techniques to get a speed up on the traverse
boundingTraverse :: (a -> (Score, b)) -> Tree a -> Best (a, b) -> Best (a,b)
boundingTraverse getScore (Node x []) best@(score,_) 
	| score' < score = (score', (x,positions))
	| otherwise      = best
	where (score', positions)  = getScore x

boundingTraverse getScore (Node x xs) best@(score,_)
	| score' < score = foldr (boundingTraverse getScore) best xs
	| otherwise      = best
	where (score', positions) = getScore x


lockingTraverse :: Int -> (a -> (Score,b)) -> MVar ForkRegister -> Tree a -> IORef (Best (a,b)) -> IO ()
lockingTraverse _ getScore _ (Node x []) best = do
	let (score',positions) = getScore x
	(score, _) <- readIORef best
	if score' < score
		then writeIORef best (score', (x,positions)) 
		else return ()

lockingTraverse forksCap getScore reg (Node x xs) best = do
	let (score',_) = getScore x
	(score, _) <- readIORef best
	if score' < score
		then mapM_ (maybeFork reg forksCap . \node -> lockingTraverse forksCap getScore reg node best) xs
		else return ()

stmTraverse :: Int -> (a -> (Score,b)) -> TVar ForkRegister -> Tree a -> TVar (Best (a,b)) -> IO ()
stmTraverse _ getScore _ (Node x []) best = do
	let (score',positions) = getScore x
	atomically $ do
		(score, _) <- readTVar best
		if score' < score
			then writeTVar best (score', (x,positions))
			else return ()

stmTraverse forksCap getScore reg (Node x xs) best = do
	let (score',_) = getScore x
	(score,_) <- atomically $ readTVar best
	if score' < score
		then mapM_ (maybeSTMFork reg forksCap . \node -> stmTraverse forksCap getScore reg node best) xs
		else return ()


