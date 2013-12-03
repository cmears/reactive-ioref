-- | A "reactive" IORef allows listeners to be attached to it.
-- When the reactive IORef is written to, the listeners --- which
-- are simple IO actions --- are executed.  Listeners can be
-- dynamically attached and detached from reactive IORefs.
-- 
-- Example of use:
-- 
-- > import Data.ReactiveIORef
-- > 
-- > main = do
-- >    -- Create the RIORef with an initial value of 0.
-- >    r <- newRIORef 0
-- >    -- When the RIORef is written to, print its new
-- >    -- value + 1 to standard output.
-- >    listen r $ \x -> print (x+1)
-- >    -- Write some new values.
-- >    writeRIORef r 123     -- r now contains 123; "124" is printed
-- >    modifyRIORef r (*2)   -- r now contains 246; "247" is printed
-- 
-- /Caveats and limitations:/
-- 
-- Absolutely not thread safe; no atomicity.
-- 
-- Potential for unwanted infinite recursion.
-- 
-- Everything is in IO, so it is not very \"functional\".
-- 
-- There is no guarantee of any kind of \"fairness\" among listeners.

module Data.ReactiveIORef
  ( 
  -- * Types
    Action
  , RIORef
  , ListenerId
  -- * Construction
  , newRIORef
  -- * Listening
  , listen
  -- * Reading and writing
  , readRIORef
  , writeRIORef
  , modifyRIORef
  -- * Derived RIORefs
  , changes
  ) where


import Control.Applicative
import Control.Monad
import Data.IORef
import qualified Data.IntMap as IM


-- |A reactive IORef.
data RIORef a = RIORef (IORef (a, IM.IntMap (Action a), Int))

-- |An 'Action' is executed whenever the 'RIORef' it is listening to
-- has a value written to it.
type Action a = a -> IO ()

-- |A way to identify listeners so that they can be detached.
newtype ListenerId = ListenerId Int

-- |Create a new RIORef with the given initial value.
newRIORef :: a -> IO (RIORef a)
newRIORef val = RIORef <$> newIORef (val, IM.empty, 0)

-- |Attach an action to a 'RIORef'.  Whenever the 'RIORef' is written
-- to, the action will be called with the new value as the argument.
-- 
-- Note that actions are called even if the new value is the same as
-- the previous value.  If you only want to know when the value is
-- different, create a derived 'RIORef' with 'changes' and listen to
-- that instead.
listen :: RIORef a -> Action a -> IO ListenerId
listen (RIORef ref) action = do
  (val, actionmap, nextid) <- readIORef ref
  let newmap = IM.insert nextid action actionmap
  writeIORef ref (val, newmap, nextid+1)
  return $ ListenerId nextid

-- |Detach a listener from an 'RIORef'.  If the listener has already
-- been detached, this does nothing.
-- 
-- Be careful: the listener /must/ have been created with this
-- 'RIORef'!  If the listener was created with a different 'RIORef'
-- you will not get an error message; you will only get erroneous
-- behaviour.
detach :: RIORef a -> ListenerId -> IO ()
detach (RIORef ref) (ListenerId lid) = do
  (val, actionmap, nextid) <- readIORef ref
  let newmap = IM.delete lid actionmap
  writeIORef ref (val, newmap, nextid)

-- |Read the current value stored in the 'RIORef'.
readRIORef :: RIORef a -> IO a
readRIORef (RIORef ref) = do
  (val, actionmap, nextid) <- readIORef ref
  return val

-- |Write a new value to the 'RIORef'.  This will cause all listeners
-- to be called.
writeRIORef :: RIORef a -> a -> IO ()
writeRIORef (RIORef ref) newval = do
  (val, actionmap, nextid) <- readIORef ref
  writeIORef ref (newval, actionmap, nextid)
  mapM_ ($newval) (IM.elems actionmap)

-- |Modify the value in the 'RIORef'.  This will cause all listeners
-- to be called.
modifyRIORef :: RIORef a -> (a -> a) -> IO ()
modifyRIORef (RIORef ref) f = do
  (val, actionmap, nextid) <- readIORef ref
  let newval = f val
  writeIORef ref (newval, actionmap, nextid)
  mapM_ ($newval) (IM.elems actionmap)


-- |Create a derived 'RIORef' which will behave the same as the
-- original 'RIORef', except that its listeners will only be called if
-- the value is different from the previously stored value.
-- 
-- For example,
-- 
-- @
-- do r <- 'newRIORef' 0
--    rc <- 'changes' r
--    'listen' r print
--    'writeRIORef' r 1
--    'writeRIORef' r 1
--    'writeRIORef' r 2
--    'writeRIORef' r 2
--    'writeRIORef' r 1
--    'writeRIORef' r 1
-- @
-- 
-- will print only \"1\", \"2\" and \"1\" --- the second consecutive
-- occurrence of each value will not cause the listener to be called.

changes :: (Eq a) => RIORef a -> IO (RIORef a)
changes rref = do
  init <- readRIORef rref
  changesRRef <- newRIORef init
  listen rref $ \newval -> do curr <- readRIORef changesRRef
                              when (newval /= curr) $ do
                                writeRIORef changesRRef newval
  return changesRRef
