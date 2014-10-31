-- WARNING: This is not a full Haskell module, it is a snippet to #include...

-- | Make a new per-thread variable.  This guarantees that no two
-- threads that execute `getTLS` on the resulting `TLS` value will
-- receive the same copy of the value.  To meet this guarantee there
-- must be AT LEAST one copy of the TLS per OS thread that calls
-- `getAll`, but their may be more.  (And in particular, there may be
-- one per lightweight IO thread.)
--
-- Thread safe.
mkTLS :: IO a -- ^ Action for creating a single copy of the TLS variable.
      -> IO (TLS a)

-- | Fetch this thread's copy of the TLS variable.  Note that all
-- values returned by this function may be immune to garbage collected
-- until `freeTLS` is called.
--
-- Thread safe.
getTLS :: TLS a -> IO a


-- | After a TLS-based computation is complete, iterate through all the 
-- 
-- NOT thread safe
allTLS :: TLS a -> IO [a]


-- | Like `allTLS`, but apply a computation directly rather than
-- building a list.
forEachTLS_ :: TLS a -> (a -> IO ()) -> IO ()
         
-- | Release all copies of the TLS variable, across all threads.  This
-- does not guarantee the storage will be freed immediately, but it
-- guarantees that the storage can be reclaimed in the future.
freeTLS :: TLS a -> IO ()
