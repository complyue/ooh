
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
    getNumB :: B'Object -> () -> IO Int
    , setNumB :: B'Object -> Int  -> IO ()
  }
type B'Object = Object B'Ctor'Args B'Ops B'Attrs

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

  opGetNum :: B'Object -> () -> IO Int
  opGetNum (Object _ _ (B'Attrs !x _)) _ = readIORef x

  opSetNum :: B'Object -> Int -> IO ()
  opSetNum (Object _ _ (B'Attrs !x _)) !v = writeIORef x v


-- pieces used to assembly object class `C` up
type C'Ctor'Args = (Int, Text)
data C'Attrs = C'Attrs {
    attrs'C'B :: !B'Attrs
    , numField'C :: !(IORef Int)
    , strField'C :: !(IORef Text)
  }
data C'Ops = C'Ops {
    ops'C'B :: !B'Ops
    , getNumC :: C'Object -> () -> IO Int
    , setNumC :: C'Object -> Int  -> IO ()
  }
type C'Object = Object C'Ctor'Args C'Ops C'Attrs

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

  opGetNumC :: C'Object -> () -> IO Int
  opGetNumC (Object _ _ (C'Attrs _ !x _)) _ = readIORef x

  opSetNumC :: C'Object -> Int -> IO ()
  opSetNumC (Object _ _ (C'Attrs _ !x _)) !v = writeIORef x v


-- application run

main :: IO ()
main = do

  !o <- classC $^ (777, "hahah")
  putStrLn $ unpack $ "obj created for class: " <> className (objClass o)

  rx <- o $. getNumC $ ()
  putStrLn $ "num c now is: " <> show rx

  o $. setNumC $ 888

  nx <- o $. getNumC $ ()
  putStrLn $ "num c then is: " <> show nx

