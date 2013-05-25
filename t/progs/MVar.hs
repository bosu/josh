import Control.Concurrent
import Control.Monad

foreign import ccall input :: Int
foreign import ccall output :: Int -> IO ()

main :: IO ()
main = do
    m <- newEmptyMVar
    forkIO $ putMVar m input
    r <- takeMVar m
    output r
