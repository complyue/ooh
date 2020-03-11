
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
-- TODO derive Eq,Ord,Hashable against classId
--      impl. Show

data Object g o a = Object {
    objId :: !Unique
    , objClass :: !(Class g o a)
    , objAttrs :: a
  }
-- TODO impl. Show and preferably customizable by concrete classes


-- | object class assembler
mkClass :: Text -> (g -> IO (Object g o a)) -> o -> Class g o a
mkClass !name !ctor !ops = Class (unsafePerformIO newUnique) name ctor ops


-- | object constructor
consObject :: Class g o a -> g -> IO (Object g o a)
consObject !c !args = classCtor c args
($^) :: Class g o a -> (g -> IO (Object g o a))
($^) = consObject


-- | object method invoker
callMethod :: Object g o a -> (o -> (Object g o a -> p -> IO b)) -> p -> IO b
callMethod !o !op !args = op (classOps $ objClass o) o args
($.) :: Object g o a -> (o -> (Object g o a -> p -> IO b)) -> (p -> IO b)
($.) = callMethod

