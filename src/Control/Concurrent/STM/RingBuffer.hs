{-|
Module      : Control.Concurrent.STM.RingBuffer
Description : A fixed buffer-size, concurrent channel implemented as a ring buffer using Software Transactional Memory (STM)
Copyright   : (c) Håkon Robbestad Gylterud, 2025
License     : LGPL-3.0-or-later
Maintainer  : hakon@gylterud.net
Stability   : experimental
Portability : non-portable (GHC extensions, STM)

This module provides a concurrency-safe ring buffer implemented
using Software Transactional Memory (STM). The buffer is lazily allocated,
and capacity enforcement is ensured through blocking operations.

The buffer allows concurrent, blocking read ('takeBuf') and write ('putBuf')
operations. Reads retries if the buffer is empty, and writes retries if the buffer
is full.

-}

module Control.Concurrent.STM.RingBuffer (RingBuffer(..),Node(..),newRingBuffer,takeBuf,putBuf) where
import Control.Concurrent.STM
import Data.List (genericReplicate)
import System.IO.Unsafe (unsafePerformIO)


-- | A node in the ring buffer, consisting of a box
--   for storing the value, and the next node in the buffer.
--   (Is essentially a stream.)
data Node a = Node { box  :: TMVar a
                   , next :: Node a }

-- | A bounded, concurrency-safe ring buffer.
data RingBuffer a = RingBuffer
                         { writeHead :: TVar (Node a)
                            -- ^ The write head points to the next node to be filed
                         , readHead :: TVar (Node a) }
                            -- ^ The read head points to the next node to be taken

-- | Contstruct the ring of nodes from a list of boxes (usually empty)
ring :: [TMVar a] -> Node a
ring = foldr Node =<< ring

-- | Create a new RingBuffer with a fixed capacity
newRingBuffer :: Integer -> STM (RingBuffer a)
newRingBuffer capacity
 | capacity <= 0 = error "A buffer must have positive capacity"
 | otherwise = do
     let r = ring $ ($ ()) <$> genericReplicate
                                   capacity
                                   (\_ -> unsafePerformIO newEmptyTMVarIO) -- Safe!
     RingBuffer <$> newTVar r
                <*> newTVar r

-- | Take an element out of the buffer. The buffer is FIFO.
--   Blocks/retries if buffer is empty.
takeBuf :: RingBuffer a -> STM a
takeBuf buffer = do
   Node b n <- readTVar (readHead buffer)
   writeTVar (readHead buffer) n 
   takeTMVar b

-- | Put an element into the buffer. The buffer is FIFO.
--   Blocks/retries if buffer is full.
putBuf :: RingBuffer a -> a -> STM ()
putBuf buffer a = do
   Node b n <- readTVar (writeHead buffer)
   putTMVar b a
   writeTVar (writeHead buffer) n

