
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
    , classCtor :: !(g -> IO a)
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
mkClass :: Text -> (g -> IO a) -> o -> Class g o a
mkClass !name !ctor !ops = Class (unsafePerformIO newUnique) name ctor ops


-- | object constructor
consObject :: Class g o a -> g -> IO (Object g o a)
consObject !c !args = do
  u     <- newUnique
  attrs <- classCtor c args
  return $ Object u c attrs
($^) :: Class g o a -> (g -> IO (Object g o a))
($^) = consObject


-- | direct method invoker
callMethod :: Object g o a -> (o -> a -> p -> IO b) -> p -> IO b
callMethod !o !op !args = op (classOps $ objClass o) (objAttrs o) args
($.) :: Object g o a -> (o -> a -> p -> IO b) -> (p -> IO b)
($.) = callMethod

-- | base method invoker
callBaseMethod
  :: Object g o a
  -> (o' -> a' -> p -> IO b)
  -> (o -> o')
  -> (a -> a')
  -> p
  -> IO b
callBaseMethod !o !op !baseOpX !baseAttrX !args =
  op (baseOpX $ classOps $ objClass o) (baseAttrX $ objAttrs o) args
($..)
  :: Object g o a
  -> (o' -> a' -> p -> IO b)
  -> (o -> o')
  -> (a -> a')
  -> p
  -> IO b
($..) = callBaseMethod

