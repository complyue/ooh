
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module Main where

import           Prelude

import           System.IO.Unsafe

import           Data.IORef
import           Data.Unique
import           Data.Text
import           Data.HashMap.Strict           as Map

import           HasObject


-- application lib


-- pieces used to assembly object class `B` up
type B'Ctor'Args = Int
data B'Attrs = B'Attrs {
      numField'B :: !(IORef Int)
    , strField'B :: !(IORef Text)
  }
data B'Ops = B'Ops {
    getNumB :: B'Attrs -> () -> IO Int
    , setNumB :: B'Attrs -> Int  -> IO ()
  }

-- object class `B` assemblied into a concrete value 
classB :: Class B'Ctor'Args B'Ops B'Attrs
classB = c
 where
  !c = mkClass "B" ctor $ B'Ops opGetNum opSetNum

  ctor :: B'Ctor'Args -> IO B'Attrs
  ctor x = do
    x' <- newIORef x
    y' <- newIORef "base str"
    return $ B'Attrs x' y'

  opGetNum :: B'Attrs -> () -> IO Int
  opGetNum (B'Attrs !x _) _ = readIORef x

  opSetNum :: B'Attrs -> Int -> IO ()
  opSetNum (B'Attrs !x _) !v = writeIORef x v


-- pieces used to assembly object class `C` up
type C'Ctor'Args = (Int, Text)
data C'Attrs = C'Attrs {
    attrs'C'B :: !B'Attrs
    , numField'C :: !(IORef Int)
    , strField'C :: !(IORef Text)
  }
data C'Ops = C'Ops {
    ops'C'B :: !B'Ops
    , getNumC :: C'Attrs -> () -> IO Int
    , setNumC :: C'Attrs -> Int  -> IO ()
  }

-- object class `C` assemblied into a concrete value 
classC :: Class C'Ctor'Args C'Ops C'Attrs
classC = c
 where
  !c = mkClass "C" ctor $ C'Ops (classOps classB) opGetNumC opSetNumC

  ctor :: C'Ctor'Args -> IO C'Attrs
  ctor (x, y) = do
    attrs'b <- classCtor classB x
    x'      <- newIORef x
    y'      <- newIORef y
    return $ C'Attrs attrs'b x' y'

  opGetNumC :: C'Attrs -> () -> IO Int
  opGetNumC (C'Attrs _ !x _) _ = readIORef x

  opSetNumC :: C'Attrs -> Int -> IO ()
  opSetNumC (C'Attrs _ !x _) !v = writeIORef x v


-- application run

main :: IO ()
main = do

  -- object construction
  !o <- classC $^ (777, "hahah")
  putStrLn $ unpack $ "obj created for class: " <> className (objClass o)

  -- calling direct methods
  cx0 <- o $. getNumC $ ()
  putStrLn $ "num at c now is: " <> show cx0
  o $. setNumC $ 888
  cx1 <- o $. getNumC $ ()
  putStrLn $ "num at c then is: " <> show cx1

  -- calling base methods
  bx0 <- (o $.. getNumB) ops'C'B attrs'C'B ()
  putStrLn $ "num at b now is: " <> show bx0
  (o $.. setNumB) ops'C'B attrs'C'B 999
  bx1 <- (o $.. getNumB) ops'C'B attrs'C'B ()
  putStrLn $ "num at b then is: " <> show bx1

