foreign import ccall input :: Int
foreign import ccall output :: Int -> IO ()

main :: IO ()
main = output input
