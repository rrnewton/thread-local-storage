{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | An implementation of TLS using the standard, Posix
-- `pthread_create_key` routine.

module Data.TLS.PThread
    -- ( TLS
    -- , mkTLS
    -- , getTLS
    -- , allTLS
    -- , forEachTLS_
    -- , freeTLS
    -- )
    where

import Control.Monad
import Control.Exception
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable(Storable(sizeOf))
    
#include "TLS_Sig.hs"
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
data TLS a = TLS { key   :: !Key
                 , mknew :: !(IO a) }
    
mkTLS new = do
  evaluate check_error
  key <- easy_make_pthread_key
  return $! TLS key new

getTLS TLS{key,mknew} = do
  p <- pthread_getspecific key
  if castStablePtrToPtr p == nullPtr then do
    a <- mknew
    sp <- newStablePtr a
    setspecific key sp
    return a
   else
    deRefStablePtr p

allTLS TLS{} = do
  error "FINISHME"

forEachTLS_ tls fn = do
  ls <- allTLS tls
  forM_ ls fn 

-- Nothing to do here... we haven't pinned anything.  Normal GC is fine.
freeTLS TLS{} =
    error "FINISHME"
