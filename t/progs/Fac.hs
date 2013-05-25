foreign import ccall input :: Int
foreign import ccall output :: Int -> IO ()

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = output $ fact input
