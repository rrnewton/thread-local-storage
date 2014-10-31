{-# LANGUAGE BangPatterns #-}

import qualified Data.TLS.PThread as PThread
import qualified Data.TLS.GHC as GHC

-- import Data.Atomics
import Data.IORef
import Foreign.Ptr
import GHC.Conc
import Control.Concurrent.MVar
import Control.Monad
import System.Mem.StableName

main :: IO ()
main = do
  putStrLn "Run a very simple TLs test"
  putStrLn $ "Key size: "++show PThread.get_pthread_key_size

  testIt "GHC" GHC.mkTLS GHC.getTLS GHC.allTLS
  testIt "PThread" PThread.mkTLS PThread.getTLS PThread.allTLS

testIt :: Show b => String -> (IO (IORef Int) -> IO t) -> (t -> IO (IORef Int)) -> (t -> IO [IORef b]) -> IO ()
testIt name mkTLS getTLS allTLS = do
  putStrLn$ "\n  Testing "++name ++" implementation: "
  putStrLn "----------------------------------------"
  tls <- mkTLS (do putStrLn "  New() called.."
                   newIORef (-1 :: Int))
  mvs <- sequence $ replicate numCapabilities newEmptyMVar
  forM_ (zip [0..] mvs) $ \(ix,mv) -> forkOn ix $ do
    r   <- getTLS tls
    n   <- readIORef r
    tid <- myThreadId
    sn  <- ssn r
    -- forM_ [1..100] $ \_ -> do writeIORef r ix; writeBarrier
    putStrLn$  "Thread "++show ix++" / "++show tid++" read "++show n++", stable name "++ sn
    writeIORef r ix
    putMVar mv ()
  forM_ mvs takeMVar -- Join
  do r   <- getTLS tls
     n   <- readIORef r
     tid <- myThreadId
     sn  <- ssn r         
     putStrLn$  "Main thread ("++show tid++") read "++show n++", stable name "++ sn
  ls <- allTLS tls
  putStrLn$ "Reading all thread-local versions, got "++show (length ls)
  ls2 <- mapM readIORef ls
  putStrLn$ "Results: "++show ls2
  ls3 <- mapM ssn ls
  putStrLn$ "Result, stable names: "++show ls3

  {- forM_ [1..(10::Int)] $ \_ -> do 
    r   <- getTLS tls
    n   <- readIORef r
    tid <- myThreadId
    sn  <- ssn r
    putStrLn$  "Read/write redux ("++show tid++"): "++show n++", stable name "++ sn
    writeIORef r 99 -}

  putStrLn "Done."


ssn :: a -> IO String
ssn a = do n <- makeStableName a
           return $ show (hashStableName n)
