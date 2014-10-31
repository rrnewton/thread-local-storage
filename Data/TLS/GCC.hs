{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

-- | An implementation of TLS using the C compiler's built-in support
-- for `__thread` variables.
-- 
-- This uses the older variable attribute, not the newer support for
-- C++11 `thread_local` variables, which have been known to have worse
-- performance.

module Data.TLS.GCC
    ( TLS
    , mkTLS
    , getTLS
    , allTLS
    , forEachTLS_
    , freeTLS
    ) where

import Control.Monad
#include "TLS_Sig.hs"
--------------------------------------------------------------------------------

-- | A thread-local variable of type `a`.
data TLS a = TLS 
    
mkTLS new = do
  error "FINISHME"

getTLS TLS{} = do
  error "FINISHME"

allTLS TLS{} = do
  error "FINISHME"

forEachTLS_ tls fn = do
  ls <- allTLS tls
  forM_ ls fn 

-- Nothing to do here... we haven't pinned anything.  Normal GC is fine.
freeTLS TLS{} =
    error "FINISHME"

    
