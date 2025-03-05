module Control.Concurrent.STM.RingBuffer (RingBuffer(..),Node(..),newRingBuffer,takeBuf,putBuf) where
import Control.Concurrent.STM
import Data.List (genericReplicate)
import System.IO.Unsafe (unsafePerformIO)

data Node a = Node { box  :: TMVar a
                   , next :: Node a }
data RingBuffer a = RingBuffer
                         { writeHead :: TVar (Node a)
                         , readHead :: TVar (Node a) }

ring :: [TMVar a] -> Node a
ring = foldr Node =<< ring

newRingBuffer :: Integer -> STM (RingBuffer a)
newRingBuffer capacity = do
     let r = ring $ ($ ()) <$> genericReplicate capacity (\_ -> unsafePerformIO newEmptyTMVarIO)
     RingBuffer <$> newTVar r
                <*> newTVar r
takeBuf :: RingBuffer a -> STM a
takeBuf buffer = do
   Node b n <- readTVar (readHead buffer)
   writeTVar (readHead buffer) n 
   takeTMVar b
   
putBuf :: RingBuffer a -> a -> STM ()
putBuf buffer a = do
   Node b n <- readTVar (writeHead buffer)
   putTMVar b a
   writeTVar (writeHead buffer) n

