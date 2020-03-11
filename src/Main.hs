
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

data C'Ops = C'Ops {
    getXXX :: C'Object -> () -> IO Int
    , setXXX :: C'Object -> Int  -> IO ()
  }

data C'Attrs = C'Attrs {
      xxx :: IORef Int
    , yyy :: IORef Text
  }

type C'Object = Object (Int, Text) C'Ops C'Attrs

classC :: Class (Int, Text) C'Ops C'Attrs
classC = c
 where
  !c = mkClass "C" ctorC $ C'Ops opGetXXX opSetXXX

  ctorC :: (Int, Text) -> IO C'Object
  ctorC (x, y) = do  -- TODO use args to initialize fields
    x' <- newIORef x
    y' <- newIORef y
    return $ Object c $ C'Attrs x' y'

  opGetXXX :: C'Object -> () -> IO Int
  opGetXXX (Object _ (C'Attrs !x _)) _ = readIORef x

  opSetXXX :: C'Object -> Int -> IO ()
  opSetXXX (Object _ (C'Attrs !x _)) !v = writeIORef x v


-- application run

main :: IO ()
main = do

  !o <- consObject classC (777, "hahah")
  putStrLn $ unpack $ "obj created for class: " <> className (objClass o)

  rx <- callOp o getXXX ()
  putStrLn $ "xxx now is: " <> show rx

  callOp o setXXX 888

  nx <- callOp o getXXX ()
  putStrLn $ "xxx then is: " <> show nx

