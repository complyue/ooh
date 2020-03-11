
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

module HasObject where

import           Prelude

import           System.IO.Unsafe

import           Data.IORef
import           Data.Unique
import           Data.Text
import           Data.HashMap.Strict           as Map


-- framework

data Class g o a = Class {
    classId :: !Unique
    , className :: !Text
    , classCtor :: !(g -> IO (Object g o a))
    , classOps :: o
  }

data Object g o a = Object {
    objClass :: !(Class g o a)
    , objAttrs :: a
  }


mkClass :: Text -> (g -> IO (Object g o a)) -> o -> Class g o a
mkClass !name !ctor !ops = Class (unsafePerformIO newUnique) name ctor ops

consObject :: Class g o a -> g -> IO (Object g o a)
consObject !c !args = classCtor c args

callOp :: Object g o a -> (o -> (Object g o a -> p -> IO b)) -> p -> IO b
callOp !o !op !args = op (classOps $ objClass o) o args

