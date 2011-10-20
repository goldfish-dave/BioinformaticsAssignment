module Forks
where

import Data.List(delete)
import Data.IORef

import Control.Concurrent
import Control.Concurrent.STM

----------------------------------------------------
-- Code Clarity
--
--type ForkRegister = ([ThreadId],Int)
type ForkRegister = Int

--emptyRegister = ([],0) :: ForkRegister
emptyRegister = 0 :: ForkRegister

type ForkSwitch = Bool

--addFork :: ForkRegister -> ThreadId -> ForkRegister
addFork :: ForkRegister -> ForkRegister
--addFork (ids,n) id = (id:ids,n+1)
addFork n = n+1

{-
dropFork :: ForkRegister -> ThreadId -> ForkRegister
dropFork (ids,n) id = (delete id ids,n-1)
-}
dropFork :: ForkRegister -> ForkRegister
dropFork n = n-1

---------------------------------------------------
-- Forking stuff


toggleLockingFork :: MVar ForkRegister -> ForkSwitch -> IO ()
toggleLockingFork mvar on = do
	register <- takeMVar mvar
	--thisId <- myThreadId
	if on
		then putMVar mvar $ addFork  register --thisId
		else putMVar mvar $ dropFork register --thisId

toggleSTMFork :: TVar ForkRegister -> ForkSwitch -> IO ()
toggleSTMFork tvar on = do
	--thisId <- myThreadId
	atomically $ do
		register <- readTVar tvar
		if on
			then writeTVar tvar $ addFork  register --thisId
			else writeTVar tvar $ dropFork register --thisId


maybeFork :: MVar ForkRegister -> Int -> IO () -> IO ()
-- will run the io action in a new fork unless
-- the number of forks represented in cap is 
-- greater than forkCap
maybeFork reg forkCap io = do
	--(_,forksCount) <- readMVar reg
	forksCount <- readMVar reg
	if forksCount < forkCap
		then (forkIO $ toggleLockingFork reg True >> io >> toggleLockingFork reg False) >> return ()
		else io

maybeSTMFork :: TVar ForkRegister -> Int -> IO () -> IO ()
maybeSTMFork reg forkCap io = do
	--(_,forksCount) <- atomically $ readTVar reg
	forksCount <- atomically $ readTVar reg
	if forksCount < forkCap
		then (forkIO $ toggleSTMFork reg True >> io >> toggleSTMFork reg False) >> return ()
		else io

waitUntil :: (a -> Bool) -> MVar a -> IO ()
waitUntil pred ref = do
	x <- readMVar ref
	if pred x
		then return ()
		else threadDelay 100000 >> waitUntil pred ref

waitUntilSTM :: (a -> Bool) -> TVar a -> IO ()
waitUntilSTM pred tvar = do
	atomically $ do
		x <- readTVar tvar
		if pred x
			then return ()
			else retry
