{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Concurrent
import Control.Monad
import Data.ReactiveIORef
import Test.HUnit
import Data.IORef

simple = do
  r <- newRIORef (99::Int)
  x <- readRIORef r
  x @?= 99

modify = do
  r <- newRIORef (0::Int)
  xs <- newRIORef []
  listen r $ \x -> modifyRIORef xs (x:)
  modifyRIORef r (+10)
  modifyRIORef r (*2)
  modifyRIORef r (+5)
  readRIORef r >>= assertEqual "modify" 25
  readRIORef xs >>= assertEqual "modify" [25,20,10]

chaining = do
  r1 <- newRIORef 0
  r2 <- newRIORef 0
  r3 <- newRIORef 0
  r4 <- newRIORef 0
  listen r1 (writeRIORef r2 . (+1))
  listen r2 (writeRIORef r3 . (+1))
  listen r3 (writeRIORef r4 . (+1))
  writeRIORef r1 1
  readRIORef r1 >>= assertEqual "chaining" 1
  readRIORef r2 >>= assertEqual "chaining" 2
  readRIORef r3 >>= assertEqual "chaining" 3
  readRIORef r4 >>= assertEqual "chaining" 4
  writeRIORef r1 1001
  readRIORef r1 >>= assertEqual "chaining" 1001
  readRIORef r2 >>= assertEqual "chaining" 1002
  readRIORef r3 >>= assertEqual "chaining" 1003
  readRIORef r4 >>= assertEqual "chaining" 1004

allWrites = do
  r <- newRIORef 99
  count <- newIORef 0
  listen r $ const (modifyIORef count (+1))
  writeRIORef r 99
  writeRIORef r 99
  writeRIORef r 99
  writeRIORef r 99
  writeRIORef r 99
  readIORef count >>= assertEqual "allWrites" 5
  readRIORef r >>= assertEqual "allWrites" 99

onlyChanges = do
  r <- newRIORef 123
  rc <- changes r
  count <- newIORef 0
  listen rc $ const (modifyIORef count (+1))
  writeRIORef r 123
  writeRIORef r 123
  writeRIORef r 99
  writeRIORef r 99
  writeRIORef r 456
  writeRIORef r 456
  writeRIORef r 456
  readIORef count >>= assertEqual "onlyChanges" 2
  readRIORef r >>= assertEqual "onlyChanges" 456
  readRIORef rc >>= assertEqual "onlyChanges" 456

threadSafe = do
  r <- newRIORef 0
  m <- newEmptyMVar
  let n = 10000
  threads <- replicateM n $ forkIO $ do modifyRIORef r (+1)
                                        putMVar m ()
  replicateM n (takeMVar m)
  readRIORef r >>= assertEqual "threadSafe" n
  isEmptyMVar m >>= assertBool "threadSafe"

detatching = do
  r <- newRIORef ()
  count1 <- newIORef 0
  count2 <- newIORef 0
  count3 <- newIORef 0
  count4 <- newIORef 0
  listener1 <- listen r $ const $ modifyRIORef (+1) count1
  listener2 <- listen r $ const $ modifyRIORef (+1) count2
  listener3 <- listen r $ const $ modifyRIORef (+1) count3
  listener4 <- listen r $ const $ modifyRIORef (+1) count4
  writeRIORef r ()
  writeRIORef r ()
  writeRIORef r ()
  detach r listener1
  writeRIORef r ()
  writeRIORef r ()
  detach r listener2
  writeRIORef r ()
  detach r listener3
  writeRIORef r ()
  detach r listener4
  writeRIORef r ()
  readIORef count1 >>= assertEqual "detaching" 3
  readIORef count2 >>= assertEqual "detaching" 5
  readIORef count3 >>= assertEqual "detaching" 6
  readIORef count4 >>= assertEqual "detaching" 7

tests = test [ simple, modify, chaining, allWrites, onlyChanges, threadSafe ]

main = runTestTT tests
