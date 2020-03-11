
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import           Prelude

import           System.IO.Unsafe

import           Data.IORef
import           Data.Unique
import           Data.Text
import           Data.HashMap.Strict           as Map

import           HasObject


-- application lib

classC :: Class
classC = c
 where
  !c = mkClass "C" ctorC [("getXXX", getAttr "xxx"), ("getYYY", getAttr "yyy")]
  ctorC :: Args -> IO Object
  ctorC !args = Object c <$> newIORef args
  getAttr :: Key -> Object -> Args -> IO Value
  getAttr !k !o _ = do
    d <- readIORef $ objAttrs o
    case Map.lookup k d of
      Nothing -> return NilValue
      Just v  -> return v


-- application run

main :: IO ()
main = do

  !o <- consObject classC
    $ Map.fromList [("nnn", IntValue 777), ("xxx", StrValue "bingo")]
  putStrLn $ unpack $ "obj created for class: " <> className (objClass o)

  rx <- callMethod o "getXXX" Map.empty
  putStrLn $ "method xxx result: " <> show rx

  ry <- callMethod o "getYYY" Map.empty
  putStrLn $ "method yyy result: " <> show ry

  -- destined to fail
  rz <- callMethod o "getZZZ" Map.empty
  putStrLn $ "method zzz result: " <> show rz
