module Main(main) where

-- we need function pointer type and free function
import Foreign.Ptr(FunPtr, freeHaskellFunPtr)

foreign import ccall output :: Int -> IO ()

-- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer
foreign import ccall "wrapper" wrap :: (Int -> Int) -> IO (FunPtr (Int -> Int))

-- import the foreign function as normal
foreign import ccall twice :: FunPtr (Int -> Int) -> Int -> IO Int

-- here's the function to use as a callback
square :: Int -> Int
square x = x * x

main :: IO ()
main = do
    squareW <- wrap square     -- make function pointer from the function
    let x = 4
    y <- twice squareW x       -- use the foreign function with our callback
    z <- twice squareW y
    output y                    -- see that it worked
    output z
    freeHaskellFunPtr squareW  -- clean up after ourselves
