import Foreign.C.String

foreign import ccall output :: CString -> IO ()

main :: IO ()
main = do
    cs <- newCString "hello"
    output cs
