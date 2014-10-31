{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- Export EVERYTHING from this internal module.
module Data.TLS.PThread.Internal where

import Control.Monad
import Control.Exception
import Data.Word
import Data.IORef
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable(Storable(sizeOf))
    
#include "../TLS_Sig.hs"
--------------------------------------------------------------------------------

type Key = Word

foreign import ccall unsafe
   get_pthread_key_size :: Int

foreign import ccall unsafe
   pthread_key_create :: Ptr Key -> Ptr () -> IO Int

foreign import ccall unsafe
   easy_make_pthread_key :: IO Key
                         
foreign import ccall unsafe
   pthread_getspecific :: Key -> IO (StablePtr a)

foreign import ccall unsafe
   pthread_setspecific :: Key -> StablePtr a -> IO Int

check_error :: ()
check_error =
 if get_pthread_key_size == sizeOf(0::Word)
 then ()
 else error "Data.TLS.PThread: internal invariant broken!  Expected pthread_key_t to be word-sized!"

{-# INLINE setspecific #-}
setspecific :: Key -> StablePtr a -> IO ()
setspecific k p = do
    code <- pthread_setspecific k p 
    unless (code == 0) (error $ "pthread_setspecific returned error code: "++show code)
           
--------------------------------------------------------------------------------

-- | A thread-local variable of type `a`.
data TLS a = TLS { key       :: {-# UNPACK #-} !Key
                 , mknew     :: !(IO a)
                 , allCopies :: {-# UNPACK #-} !(IORef [StablePtr a]) }
    
mkTLS new = do
  evaluate check_error
  key  <- easy_make_pthread_key
  allC <- newIORef []
  return $! TLS key new allC

getTLS TLS{key,mknew,allCopies} = do
  p <- pthread_getspecific key
  if castStablePtrToPtr p == nullPtr then do
    a <- mknew
    sp <- newStablePtr a
    setspecific key sp
    atomicModifyIORef' allCopies (\l -> (sp:l,()))
    return a
   else
    deRefStablePtr p

allTLS TLS{allCopies} = do 
    ls <- readIORef allCopies
    mapM deRefStablePtr ls

forEachTLS_ tls fn = do
  ls <- allTLS tls
  forM_ ls fn 

freeTLS TLS{allCopies} = do 
    ls <- readIORef allCopies
    mapM_ freeStablePtr ls
