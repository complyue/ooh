# Ooh! Object Oriented Haskell

I never imagined **OO** stylish program can be written in **Haskel**
in such a way so straight forward!

Constructing object:
```haskell
!o <- classC $^ (777, "hahah")
```

Calling direct methods:
```haskell
  cx0 <- o $. getNumC $ ()
  o $. setNumC $ 888
```

Calling base methods:
```haskell
  bx0 <- (o $.. cast'C'as'B) getNumB ()
  (o $.. cast'C'as'B) setNumB 999
```

Though the object class definition appears verbose as hand-crafted for now,
I believe **Template Haskell** has much to offer for better syntax sugar.

Base class:
```haskell
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
```

Derived class:
```haskell
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
```

Caster:
```haskell
cast'C'as'B :: (C'Ops -> B'Ops, C'Attrs -> B'Attrs)
cast'C'as'B = (ops'C'B, attrs'C'B)
```

Machinery:
```haskell

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
  -> ((o -> o'), (a -> a'))
  -> (o' -> a' -> p -> IO b)
  -> p
  -> IO b
callBaseMethod !o (opsX, attrsX) !op !args =
  op (opsX $ classOps $ objClass o) (attrsX $ objAttrs o) args
($..)
  :: Object g o a
  -> ((o -> o'), (a -> a'))
  -> (o' -> a' -> p -> IO b)
  -> p
  -> IO b
($..) = callBaseMethod

```
