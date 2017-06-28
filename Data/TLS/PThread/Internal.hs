{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Like "Data.TLS.PThread", but this also exports internal functionality
-- not exposed in the public interface.
--
-- There are no API guaranteees whatsoever for this module, so use it with
-- with caution.
module Data.TLS.PThread.Internal where

import Control.Monad
import Control.Exception
import Data.IORef
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable(Storable(peek, poke, sizeOf))

#if !(MIN_VERSION_base(4,8,0))
import Data.Word (Word)
#endif

#include "../TLS_Sig.hs"
--------------------------------------------------------------------------------

type Key = Word

foreign import ccall unsafe
   get_pthread_key_size :: Int

foreign import ccall unsafe
   pthread_key_create :: Ptr Key -> Ptr () -> IO Int

foreign import ccall unsafe
   easy_make_pthread_key :: FunPtr (Ptr (StablePtr a) -> IO ()) -> IO Key

foreign import ccall unsafe
   pthread_getspecific :: Key -> IO (Ptr (StablePtr a))

foreign import ccall unsafe
   pthread_setspecific :: Key -> (Ptr (StablePtr a)) -> IO Int

foreign import ccall unsafe
   pthread_key_delete :: Key -> IO Int

check_error :: ()
check_error =
--  if get_pthread_key_size == sizeOf(0::Word)
 if get_pthread_key_size <= sizeOf(0::Word)
 then ()
 else error $ "Data.TLS.PThread: internal invariant broken!  Expected pthread_key_t to be word-sized!\n"
             ++"Instead it was: "++show get_pthread_key_size


{-# INLINE setspecific #-}
setspecific :: Key -> Ptr (StablePtr a) -> IO ()
setspecific k p = do
    code <- pthread_setspecific k p
    unless (code == 0) (error $ "pthread_setspecific returned error code: "++show code)

{-# INLINE delete #-}
delete :: Key -> IO ()
delete k = do
    code <- pthread_key_delete k
--    putStrLn $ "KEY DELETED: "++show k
    unless (code == 0) (error $ "pthread_key_delete returned error code: "++show code)
    return ()


--------------------------------------------------------------------------------

-- | A thread-local variable of type `a`.
data TLS a = TLS { key       :: {-# UNPACK #-} !Key
                 , mknew     :: !(IO a)
                 , allCopies :: {-# UNPACK #-} !(IORef [Ptr (StablePtr a)])
                 , destructor :: FunPtr (Ptr (StablePtr a) -> IO ())
                 }

mkTLS new = mkTLSWithDestructor new Nothing

foreign import ccall "wrapper" wrapDestructor
  :: (Ptr (StablePtr a) -> IO ()) -> IO (FunPtr (Ptr (StablePtr a) -> IO ()))

-- | Like mkTLS but offers a destructor function which is called by any thread
-- that created a copy of the variable when the thread exits. The destructor
-- executes on a thread only if the thread exits before 'freeAllTLS' is called
-- on the TLS variable.
mkTLSWithDestructor new mdestructor = do
  evaluate check_error
  wrap <- maybe
    (return nullFunPtr)
    (\dfun -> wrapDestructor (peek >=> deRefStablePtr >=> dfun))
    mdestructor
  key  <- easy_make_pthread_key wrap
--   putStrLn $ "KEY CREATED: "++show key
  allC <- newIORef []
  return $! TLS key new allC wrap

-- Note [Ptr indirection]
--
-- getTLS check that the value of the pthread_key is not null
-- to determine if a new copy of the TLS variable needs to be created.
-- However, @castStablePtrToPtr <$> newStablePtr a@ might return @nullPtr@
-- as the API of StablePtr does not guarantee that the resulting pointer is a
-- valid address.
--
-- To ensure that getTLS does not create two copies for the same thread,
-- we wrap the StablePtr in a malloc'ed Ptr and store that with setspecific.
--
-- Another undesirable consequence of confusing a StablePtr with nullPtr is
-- that the destructor function of the pthread_key wouldn't be called in that
-- case.

getTLS TLS{key,mknew,allCopies} = do
  p <- pthread_getspecific key
  if p == nullPtr then do
    a <- mknew
    sp <- newStablePtr a
    -- See note [Ptr indirection]
    ptr <- malloc
    poke ptr sp
    setspecific key ptr
    atomicModifyIORef' allCopies (\l -> (ptr:l,()))
    return a
   else
    peek p >>= deRefStablePtr

allTLS TLS{allCopies} = do
    ls <- readIORef allCopies
    mapM (peek >=> deRefStablePtr) ls

forEachTLS_ tls fn = do
  ls <- allTLS tls
  forM_ ls fn

freeAllTLS TLS{key, allCopies, destructor} = do
    ls <- readIORef allCopies
    delete key
    when (destructor /= nullFunPtr) (freeHaskellFunPtr destructor)
    mapM_ (\p -> peek p >>= freeStablePtr >> free p) ls
