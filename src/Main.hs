
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
    getXXX :: (Object C'Ops C'Attrs) -> Args -> IO Value
    , setXXX :: (Object C'Ops C'Attrs) -> Args -> IO ()
  }

data C'Attrs = C'Attrs {
      xxx :: IORef Value
    , yyy :: IORef Value
  }

classC :: Class C'Ops C'Attrs
classC = c
 where
  !c = mkClass "C" ctorC $ C'Ops opGetXXX opSetXXX

  ctorC :: Args -> IO (Object C'Ops C'Attrs)
  ctorC args = do  -- TODO use args to initialize fields
    x <- newIORef $ IntValue 777
    y <- newIORef $ StrValue "hhh"
    return $ Object c $ C'Attrs x y

  opGetXXX :: (Object C'Ops C'Attrs) -> Args -> IO Value
  opGetXXX (Object _ (C'Attrs !x _)) !_args = readIORef x

  opSetXXX :: (Object C'Ops C'Attrs) -> Args -> IO ()
  opSetXXX (Object _ (C'Attrs !x _)) !_args = writeIORef x $ StrValue "888"


-- application run

main :: IO ()
main = do

  !o <- consObject classC
    $ Map.fromList [("nnn", IntValue 777), ("xxx", StrValue "bingo")]
  putStrLn $ unpack $ "obj created for class: " <> className (objClass o)

  rx <- callOp o getXXX Map.empty
  putStrLn $ "xxx now is: " <> show rx

  callOp o setXXX Map.empty

  nx <- callOp o getXXX Map.empty
  putStrLn $ "xxx then is: " <> show nx

