import Foreign.Ptr
import Control.Concurrent
import Control.Monad

data ReqResp
data Request
data Unregister

type Registrator a = FunPtr (Ptr a -> IO ()) -> IO (Ptr Unregister)
type UnregInfo a = (FunPtr (Ptr a -> IO ()), Ptr Unregister)

foreign import ccall start_server :: Registrator ReqResp
foreign import ccall response :: Ptr ReqResp -> Ptr Request
foreign import ccall answer :: Ptr Request -> IO ()
foreign import ccall call_unregister :: Ptr Unregister -> IO ()

foreign import ccall "wrapper" wrap
    :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

type Rendezvous a b = MVar (Ptr a, b)

register :: b -> Rendezvous a b -> Registrator a -> IO (UnregInfo a)
register b m reg = do
    cb <- wrap (go m)
    un <- reg cb
    return (cb, un)
    where go m p = putMVar m (p, b)

wait :: Rendezvous a b -> IO (Ptr a, b)
wait = takeMVar

unregister :: UnregInfo a -> IO ()
unregister (cb, un) = do
    call_unregister un
    freeHaskellFunPtr cb

main :: IO ()
main = do
    m <- newEmptyMVar
    un <- register () m start_server
    (r, _) <- wait m
    answer $ response r
    unregister un
