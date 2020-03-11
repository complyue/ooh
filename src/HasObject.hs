
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
type Ctor = Args -> IO Object
type Method = Object -> Args -> IO Value

data Class = Class {
    classId :: !Unique
    , className :: !Text
    , classCtor :: !Ctor
    , classMethods :: !(Map.HashMap Text Method)
  }

data Object = Object {
    objClass :: !Class
    , objAttrs :: !(IORef Attrs)
  }
type Attrs = Map.HashMap Key Value


mkClass :: Text -> Ctor -> [(Text, Method)] -> Class
mkClass !name !ctor !mths =
  Class (unsafePerformIO newUnique) name ctor $ Map.fromList mths

consObject :: Class -> Args -> IO Object
consObject !c !args = classCtor c args

callMethod :: Object -> Text -> Args -> IO Value
callMethod !o !mn !args = case Map.lookup mn (classMethods $ objClass o) of
  Nothing   -> error $ unpack $ "No such method: " <> mn
  Just !mth -> mth o args

