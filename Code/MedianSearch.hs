module MedianSearch
where

import Distances
import DNA
import MotifTrees

import Data.Tree
import Data.Foldable (minimumBy)

import Data.IORef

import Control.Concurrent
import Control.Concurrent.STM

import Forks

infinity = maxBound :: Int
seed = (infinity, undefined)

simpleMedianSearch :: DNA -> Int -> (Motif,[Position])
simpleMedianSearch dna l = snd $ simpleTraverse getScore tree seed
	where
		getScore = scoreFunction dna l
		tree = searchTree l


boundingMedianSearch :: DNA -> Int -> (Motif, [Position])
boundingMedianSearch dna l = snd $ boundingTraverse getScore tree seed 
	where
		getScore = scoreFunction dna l
		tree = searchTree l

lockingMedianSearch :: Int -> DNA -> Int -> IO (Motif, [Position])
lockingMedianSearch forksCap dna l = do
	let tree = searchTree l
	let getScore = scoreFunction dna l
	reg <- newMVar emptyRegister
	best <- newIORef seed
	maybeFork reg forksCap $ lockingTraverse forksCap getScore reg tree best
	waitUntil (== emptyRegister) reg
	readIORef best >>= return . snd

stmMedianSearch :: Int -> DNA -> Int -> IO (Motif, [Position])
stmMedianSearch forksCap dna l = do
	let tree = searchTree l
	let getScore = scoreFunction dna l
	reg <- atomically $ newTVar emptyRegister
	best <- atomically $ newTVar seed
	maybeSTMFork reg forksCap $ stmTraverse forksCap getScore reg tree best
	waitUntilSTM (== emptyRegister) reg
	atomically $ readTVar best >>= return . snd

