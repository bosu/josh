module Main(main) where

fib :: Int -> Int
fib n =
  if n < 2 then
    n
  else
    fib (n - 1) + fib (n - 2)

foreign import ccall input :: Int
foreign import ccall output :: Int -> IO ()

main :: IO ()
main = output $ fib input
