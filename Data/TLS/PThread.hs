{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

-- | An implementation of TLS using the standard, Posix
-- `pthread_create_key` routine.

module Data.TLS.PThread
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

