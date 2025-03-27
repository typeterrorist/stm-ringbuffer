{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Control.Concurrent.STM.RingBuffer.Test
Description : QuickCheck tests for the STM-based RingBuffer
-}

module Control.Concurrent.STM.RingBuffer.Test where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.RingBuffer
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Applicative
import Data.List (genericLength, genericReplicate)

-- | Converts STM a to STM (Maybe a) without blocking
try :: (Alternative f, Alternative g) => f a -> f (g a)
try = (<|> pure empty) . (pure <$>)

prop_allocate :: Integer -> Property
prop_allocate capacity = capacity > 0 ==> monadicIO $ do
    buffer <- run $ atomically $ (newRingBuffer capacity :: STM (RingBuffer String))
    run $ atomically $ putRB buffer "x"
    assert True

-- | Test that a single-threaded sequence of put/take preserves FIFO ordering
prop_fifo :: [Int] -> Property
prop_fifo xs = monadicIO $ do
    let capacity = max 1 (genericLength xs)
    buffer <- run $ atomically $ newRingBuffer capacity
    run $ atomically $ mapM_ (putRB buffer) xs
    ys <- run $ atomically $ mapM (const $ takeRB buffer) xs
    assert (xs == ys)

-- | Test that taking from an empty buffer blocks until an element is available
prop_blocking_take :: Int -> Property
prop_blocking_take x = monadicIO $ do
    buffer <- run $ atomically (newRingBuffer 1)
    Nothing <- run $ atomically $ try $ takeRB buffer
    run $ atomically (putRB buffer x)
    y <- run $ atomically (takeRB buffer)
    assert (x == y)

-- | Test that writing to a full buffer blocks until an element is taken
prop_blocking_put :: [Int] -> Int -> Property
prop_blocking_put xs y = xs /= [] ==> monadicIO $ do
    buffer <- run $ atomically (newRingBuffer (genericLength xs))
    run $ atomically $ mapM_ (putRB buffer) xs
    -- Should not be able to put another element while full
    Nothing <- run $ atomically (try (putRB buffer y))
    (x:xs') <- pure xs
    x' <- run $ atomically (takeRB buffer)
    assert $ x' == x
    run $ atomically (putRB buffer y)
    xs'' <- run $ atomically $ mapM (const (takeRB buffer)) xs'
    assert $ xs'' == xs'
    y' <- run $ atomically (takeRB buffer)
    assert (y == y')

-- | Test creating a large buffer (3 million capacity) and using it minimally
prop_large_buffer :: [Int] -> Property
prop_large_buffer smallList = monadicIO $ do
    let capacity = 300000000 :: Integer
    buffer <- run $ atomically (newRingBuffer capacity)
    run $ atomically $ mapM_ (putRB buffer) smallList
    ys <- run $ atomically $ mapM (const $ takeRB buffer) smallList
    assert (smallList == ys)

prop_test_productive_thread :: Integer -> Property
prop_test_productive_thread capacity = capacity > 0 ==> monadicIO $ do
   buffer <- run $ atomically (newRingBuffer capacity)
   _ <- run $ forkIO $ mapM_ (atomically . putRB buffer) [1..]
   let l = 3*capacity + 7
   xs <- run $ sequence $ genericReplicate l (atomically $ takeRB buffer)
   assert (xs == [1..l])

-- | Test creating a large buffer (3 million capacity) using newRingBufferIO and using it minimally
prop_large_buffer_io :: [Int] -> Property
prop_large_buffer_io smallList = monadicIO $ do
    let capacity = 300000000 :: Integer
    buffer <- run $ newRingBufferIO capacity
    run $ atomically $ mapM_ (putRB buffer) smallList
    ys <- run $ atomically $ mapM (const $ takeRB buffer) smallList
    assert (smallList == ys)

return [] -- Needed for QuickCheck's template haskell

tests :: IO Bool
tests = $quickCheckAll

