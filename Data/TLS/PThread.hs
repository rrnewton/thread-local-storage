{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | An implementation of TLS using the standard, Posix
-- `pthread_create_key` routine.

module Data.TLS.PThread
    ( TLS
    , mkTLS
    , getTLS
    , allTLS
    , forEachTLS_
    , freeTLS
    )
    where

-- Same as the internal module but with limited export:
import Data.TLS.PThread.Internal
