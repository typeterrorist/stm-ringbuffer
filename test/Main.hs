module Main (main) where
import Control.Concurrent.STM.RingBuffer.Test (tests)

main :: IO ()
main = do
    success <- tests
    if success then putStrLn "All tests passed!" else error "Some tests failed."

