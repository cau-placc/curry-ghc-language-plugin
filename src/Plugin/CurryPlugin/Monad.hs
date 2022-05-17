{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# OPTIONS_GHC -Wno-orphans            #-}
{-|
Module      : Plugin.CurryPlugin.Monad
Description : Convenience wrapper for the effect
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains the actual monad used by the plugin and a few
convenicence functions.
The monad type is a wrapper over the
'Lazy' type from 'Plugin.Effect.CurryEffect'.
-}
module Plugin.CurryPlugin.Monad
  ( Curry(..), type (-->)(..), (?), failed, share
  , SearchMode(..)
  , Normalform(..), modeOp, allValues, allValuesNF
  , NondetTag(..)
  , liftCurry1, liftCurry2
  , app, apply2, apply2Unlifted, apply3
  , bind, rtrn, rtrnS, rtrnFunc, fmp, shre, shreTopLevel, seqValue
  , rtrnFuncUnsafePoly, appUnsafePoly )
  where

import Language.Haskell.TH.Syntax hiding (lift)
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Codensity
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef
import Data.Function
import System.IO.Unsafe
import Unsafe.Coerce
import GHC.Types.Unique
import GHC.Types.Unique.Supply
import GHC.Exts
import GHC.Generics as Gen

import Plugin.Effect.Classes
import Plugin.CurryPlugin.Tree
import Plugin.Effect.Annotation

type Heap a = IntMap a
type ID = UniqSupply

splitID :: ID -> (ID, ID)
splitID = splitUniqSupply

instance Eq UniqSupply where
  (==) = (==) `on` (getKey . uniqFromSupply)

instance Ord UniqSupply where
  compare = compare `on` (getKey . uniqFromSupply)

emptyHeap :: Heap (Bool, a)
emptyHeap = IntMap.empty

insertHeap :: ID -> (Bool, a) -> Heap (Bool, a) -> Heap (Bool, a)
insertHeap i = IntMap.insert (getKey (uniqFromSupply i))

lookupHeap :: ID -> Heap (Bool, a) -> Maybe (Bool, a)
lookupHeap i = IntMap.lookup (getKey (uniqFromSupply i))

data MemoState = MemoState ID (Set ID)

data STag = Unshared | Shared
  deriving Eq

data SVal a = SVal {
    valTag :: STag,
    valVal :: a
  } deriving Functor

newtype Curry a = Curry {
    unCurry :: StateT MemoState (Codensity Tree) (SVal a)
  } deriving anyclass (SharingTop, MonadPlus)

instance Functor Curry where
  fmap f (Curry m) = Curry $ fmap (fmap f) m

instance Applicative Curry where
  pure x = Curry $ return $ SVal Unshared x
  Curry mf <*> Curry ma = Curry ((\f a -> SVal Unshared ((valVal f) (valVal a))) <$> mf <*> ma)

instance Monad Curry where
  Curry ma >>= f = Curry $ do
    SVal _ a <- ma
    unCurry (f a)

instance Alternative Curry where
  empty = Curry (lift (lift Failed))
  Curry ma1 <|> Curry ma2 = Curry $ do
    MemoState b1 p1 <- get
    let (i1, i2) = splitID b1
    let parents = Set.insert b1 p1
    (put (MemoState i1 parents) >> ma1)
      <|> (put (MemoState i2 parents) >> ma2)

instance {-# INCOHERENT #-} NormalformGen Curry Gen.V1 Gen.V1 where
  nfGen _ = undefined
  liftEGen _ = undefined

instance {-# INCOHERENT #-} NormalformGen Curry Gen.U1 Gen.U1 where
  nfGen mx = mx >>= \case
    Gen.U1 -> rtrnS Gen.U1
  liftEGen mx = mx >>= \case
    Gen.U1 -> rtrnS Gen.U1

instance {-# INCOHERENT #-} (NormalformGen Curry f1 g1, NormalformGen Curry f2 g2) =>
  NormalformGen Curry (f1 Gen.:+: f2) (g1 Gen.:+: g2) where
    nfGen mx = mx >>= \case
      Gen.L1 x -> Gen.L1 <$> nfGen (rtrnS x)
      Gen.R1 x -> Gen.R1 <$> nfGen (rtrnS x)
    liftEGen mx = mx >>= \case
      Gen.L1 x -> Gen.L1 <$> liftEGen (rtrnS x)
      Gen.R1 x -> Gen.R1 <$> liftEGen (rtrnS x)

instance {-# INCOHERENT #-} (NormalformGen Curry f1 g1, NormalformGen Curry f2 g2) =>
  NormalformGen Curry (f1 Gen.:*: f2) (g1 Gen.:*: g2) where
    nfGen mx = mx >>= \case
      x Gen.:*: y -> (Gen.:*:) <$> nfGen (rtrnS x) <*> nfGen (rtrnS y)
    liftEGen mx = mx >>= \case
      x Gen.:*: y -> (Gen.:*:) <$> liftEGen (rtrnS x) <*> liftEGen (rtrnS y)

instance {-# INCOHERENT #-} (Normalform Curry a b) =>
  NormalformGen Curry (Gen.K1 i (Curry a)) (Gen.K1 j b) where
    nfGen mx = mx >>= \case
      Gen.K1 x -> Gen.K1 <$> nf x
    liftEGen mx = mx >>= \case
      Gen.K1 x -> Gen.K1 <$> rtrnS (liftE (rtrnS x))

instance {-# INCOHERENT #-} (NormalformGen Curry f g) =>
  NormalformGen Curry (Gen.M1 i t f) (Gen.M1 j h g) where
    nfGen mx = mx >>= \case
      Gen.M1 x -> Gen.M1 <$> nfGen (return x)
    liftEGen mx = mx >>= \case
      Gen.M1 x -> Gen.M1 <$> liftEGen (rtrnS x)

lookupTaskMap :: Heap (Bool, a) -> ID -> Set ID -> Maybe (Bool, a)
lookupTaskMap h i p = msum (map (`lookupHeap` h) (i : Set.toList p))

instance Sharing Curry where
  type ShareConstraints Curry a = Shareable Curry a
  share :: forall a. Shareable Curry a => Curry a -> Curry (Curry a)
  share (Curry ma) = memo $ Curry $ ma >>= \case
         SVal Shared   v -> return (SVal Shared v)
         SVal Unshared v -> unCurry (shareArgs v)
    where
      memo :: Curry a -> Curry (Curry a)
      memo (Curry ms) = Curry $ do
        MemoState b1 _ <- get
        -- not allowed to float out!
        let mapRef = unsafePerformIO (newIORef (noinline const (emptyHeap @a) b1))
        return $ SVal Shared $ Curry $ do
          MemoState b2 p2 <- get
          let m = unsafePerformIO (readIORef (noinline const mapRef b2))
          case lookupTaskMap m b2 p2 of
            Just (nd, res)
              | nd -> let i = snd (splitID b2)
                      in put (MemoState i (Set.insert b2 p2))
                         >> return (SVal Shared res)
              | otherwise -> return (SVal Shared res)
            Nothing  -> do
              SVal _ a <- ms
              MemoState b3 _ <- get
              let whereToInsert = if b2 == b3 then b1 else b3
              let inserted = insertHeap whereToInsert (b2 /= b3, a) m
              unsafePerformIO (writeIORef mapRef inserted) `seq` return (SVal Shared a)

{-# INLINE[0] bind #-}
bind :: Curry a -> (a -> Curry b) -> Curry b
bind = (>>=)

{-# INLINE[0] rtrn #-}
rtrn :: a -> Curry a
rtrn = pure

{-# INLINE[0] rtrnS #-}
rtrnS :: a -> Curry a
rtrnS = Curry . pure . SVal Shared

{-# INLINE[0] rtrnFunc #-}
rtrnFunc :: (Curry a -> Curry b) -> Curry (a --> b)
rtrnFunc = rtrnS . Func

{-# INLINE[0] app #-}
app :: Curry (a --> b) -> Curry a -> Curry b
app mf ma = mf >>= \(Func f) -> f ma

-- HACK:
-- RankNTypes are not really supported for various reasons,
-- but to test rewrite rules, we needed them to be supported at least
-- when the functions with RankN types are used and defined in the same module.
-- However, imagine we have a lambda with a (rank 2) type
-- (forall x. blah) -> blub.
-- Its lifted variant is something like
-- (forall x. blah') --> blub'
-- If we "unpack" the (-->) type constructor we get
-- m (forall x. blah') -> m blub'
-- This is bad, because the lifted type of the argument (forall x. blah)
-- is (forall x. m blah') and not m (forall x. blah').
-- To remedy this, we provide the following two functions using unsafeCoerce to
-- accomodate such a RankN type.
{-# INLINE[0] rtrnFuncUnsafePoly #-}
rtrnFuncUnsafePoly :: forall a b a'. (a' -> Curry b) -> Curry (a --> b)
rtrnFuncUnsafePoly f = pure (Func (unsafeCoerce f :: Curry a -> Curry b))

{-# INLINE[0] appUnsafePoly #-}
appUnsafePoly :: forall a b a'. Curry (a --> b) -> a' -> Curry b
appUnsafePoly mf ma = mf >>= \(Func f) -> (unsafeCoerce f :: a' -> Curry b) ma

{-# INLINE[0] fmp #-}
fmp :: (a -> b) -> Curry a -> Curry b
fmp = fmap

{-# INLINE[0] shre #-}
shre :: Shareable Curry a => Curry a -> Curry (Curry a)
shre = share

{-# INLINE[0] shreTopLevel #-}
shreTopLevel :: (Int, String) -> Curry a -> Curry a
shreTopLevel = shareTopLevel

{-# INLINE seqValue #-}
seqValue :: Curry a -> Curry b -> Curry b
seqValue a b = a >>= \a' -> a' `seq` b

{-# RULES
"bind/rtrn"    forall f x. bind (rtrn x) f = f x
"bind/rtrnS"   forall f x. bind (rtrnS x) f = f x
"app/rtrnFunc" forall f x. app (rtrnFunc f) x = f x
"shreTopLevel" forall x i. shreTopLevel i x = x
  #-}
  -- "bind/rtrn'let"   forall e x. let b = e in rtrn x = rtrn (let b = e in x)

-- | Curryerministic failure
failed :: Shareable Curry a => Curry a
failed = mzero

infixr 0 ?
{-# INLINE (?) #-}
-- | Curryerministic choice
(?) :: Shareable Curry a => Curry (a --> a --> a)
(?) = rtrnFunc $ \t1 -> rtrnFunc $ \t2 -> t1 `mplus` t2

-- | Enumeration of available search modes.
data SearchMode = DFS -- ^ depth-first search
                | BFS -- ^ breadth-first search
  deriving Lift

-- | Function to map the search type to the function implementing it.
modeOp :: SearchMode -> Tree a -> [a]
modeOp DFS = dfs
modeOp BFS = bfs

-- | Collect the results of a Curryerministic computation
-- as their normal form in a tree.
allValuesNF :: Normalform Curry a b
            => Curry a -> Tree b
allValuesNF = allValues . nf

-- | Collect the results of a Curryerministic computation in a tree.
allValues :: Curry a -> Tree a
allValues c = valVal <$> runCodensity (evalStateT (unCurry c) $ MemoState (unsafePerformIO (mkSplitUniqSupply 'a')) Set.empty) return

infixr 0 -->
newtype a --> b = Func (Curry a -> Curry b)

instance (Sharing m) => Shareable m (a --> b) where
  shareArgs (Func f) = fmap Func (shareArgs f)

instance (Normalform Curry a1 a2, Normalform Curry b1 b2)
  => Normalform Curry (a1 --> b1) (a2 -> b2) where
    nf    mf =
      mf >> return (error "Plugin Error: Cannot capture function types")
    liftE mf = do
      f <- mf
      return (Func (liftE . fmap f . nf))

-- | Lift a unary function with the lifting scheme of the plugin.
liftCurry1 :: (a -> b) -> Curry (a --> b)
liftCurry1 f = rtrnFunc (\a -> a >>= \a' -> Curry $ return $ SVal Shared (f a'))

-- | Lift a 2-ary function with the lifting scheme of the plugin.
liftCurry2 :: (a -> b -> c) -> Curry (a --> b --> c)
liftCurry2 f = rtrnFunc (\a  -> rtrnFunc (\b  ->
                a >>=  \a' -> b >>=     \b' -> Curry $ return $ SVal Shared (f a' b')))

-- | Apply a lifted 2-ary function to its lifted arguments.
apply2 :: Curry (a --> b --> c) -> Curry a -> Curry b -> Curry c
apply2 f a b = app f a >>= \(Func f') -> f' b

-- | Apply a lifted 2-ary function to its arguments, where just the
-- first argument has to be lifted.
apply2Unlifted :: Curry (a --> b --> c)
               -> Curry a -> b -> Curry c
apply2Unlifted f a b = app f a >>= \(Func f') -> f' (return b)

-- | Apply a lifted 3-ary function to its lifted arguments.
apply3 :: Curry (a --> b --> c --> d)
       -> Curry a -> Curry b -> Curry c -> Curry d
apply3 f a b c = apply2 f a b >>= \(Func f') -> f' c
