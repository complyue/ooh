
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

type Key = Text
data Value = NilValue
    | IntValue !Int
    | StrValue !Text
    -- ...
  deriving (Eq,Ord,Show)
type Args = Map.HashMap Key Value


data Class o a = Class {
    classId :: !Unique
    , className :: !Text
    , classCtor :: !(Args -> IO (Object o a))
    , classOps :: o
  }

data Object o a = Object {
    objClass :: !(Class o a)
    , objAttrs :: a
  }


mkClass :: Text -> (Args -> IO (Object o a)) -> o -> Class o a
mkClass !name !ctor !ops = Class (unsafePerformIO newUnique) name ctor ops

consObject :: Class o a -> Args -> IO (Object o a)
consObject !c !args = classCtor c args

callOp :: Object o a -> (o -> (Object o a -> Args -> IO b)) -> Args -> IO b
callOp !o !op !args = op (classOps $ objClass o) o args

