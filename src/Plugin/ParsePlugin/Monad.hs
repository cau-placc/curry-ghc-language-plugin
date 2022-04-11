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
{-|
Module      : Plugin.ParsePlugin.Monad
Description : Convenience wrapper for the effect
Copyright   : (c) Kai-Oliver Prott (2020)
Maintainer  : kai.prott@hotmail.de

This module contains the actual monad used by the plugin and a few
convenicence functions.
The monad type is a wrapper over the
'Lazy' type from 'Plugin.Effect.CurryEffect'.
-}
module Plugin.ParsePlugin.Monad
  ( Parse(..), type (-->)(..), share
  , Normalform(..)
  , EffectTag(..)
  , parse
  , anyChar, isEOF, eof
  , liftParse1, liftParse2
  , app, apply2, apply2Unlifted, apply3
  , bind, rtrn, rtrnFunc, fmp, shre, shreTopLevel, seqValue
  , rtrnFuncUnsafePoly, appUnsafePoly )
  where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Unsafe.Coerce

import Plugin.Effect.Classes
import Plugin.Effect.Annotation
import Plugin.Effect.Transformers

newtype ParseP a = ParseP { unParseP :: String -> [(String, a)] }
  deriving Functor

instance Applicative ParseP where 
  pure a = ParseP (\s -> pure (s, a))

  ParseP fa <*> ParseP aa = ParseP $ \s -> 
    concatMap (\(s', f) -> map (second f) (aa s')) (fa s)

instance Monad ParseP where 
  ParseP am >>= f = ParseP $ \s ->
    concatMap (\(s', a) -> unParseP (f a) s') (am s)

instance Alternative ParseP where 
  empty = ParseP (const [])

  ParseP a <|> ParseP b = ParseP $ \s -> 
    a s ++ b s

instance MonadPlus ParseP where

-- | The actual monad for parsing used by the plugin.
newtype Parse a = Parse { unParse :: NameT Parse ParseP a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Sharing)
    via NameT Parse ParseP
  deriving anyclass (SharingTop)

parse :: Normalform Parse a1 a2 => Parse a1 -> String -> [(String, a2)]
parse x s = unParseP (runNameT (unParse (nf x))) s 

anyChar :: Parse Char 
anyChar = Parse $ lift $ ParseP $ \s -> 
  case s of 
    []     -> []
    (x:xs) -> [(xs, x)]

eof :: Parse ()
eof = Parse $ lift $ ParseP $ \s ->
  case s of
    [] -> [([], ())]
    _  -> []

isEOF :: Parse Bool 
isEOF = Parse $ lift $ ParseP $ \s ->
  case s of
    [] -> [([], True)]
    xs -> [(xs, False)]

{-# INLINE[0] bind #-}
bind :: Parse a -> (a -> Parse b) -> Parse b
bind = (>>=)

{-# INLINE[0] rtrn #-}
rtrn :: a -> Parse a
rtrn = pure

{-# INLINE[0] rtrnFunc #-}
rtrnFunc :: (Parse a -> Parse b) -> Parse (a --> b)
rtrnFunc = pure . Func

{-# INLINE[0] app #-}
app :: Parse (a --> b) -> Parse a -> Parse b
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
rtrnFuncUnsafePoly :: forall a b a'. (a' -> Parse b) -> Parse (a --> b)
rtrnFuncUnsafePoly f = pure (Func (unsafeCoerce f :: Parse a -> Parse b))

{-# INLINE[0] appUnsafePoly #-}
appUnsafePoly :: forall a b a'. Parse (a --> b) -> a' -> Parse b
appUnsafePoly mf ma = mf >>= \(Func f) -> (unsafeCoerce f :: a' -> Parse b) ma

{-# INLINE[0] fmp #-}
fmp :: (a -> b) -> Parse a -> Parse b
fmp = fmap

{-# INLINE[0] shre #-}
shre :: Parse a -> Parse (Parse a)
shre = share

{-# INLINE[0] shreTopLevel #-}
shreTopLevel :: (Int, String) -> Parse a -> Parse a
shreTopLevel = shareTopLevel

{-# INLINE seqValue #-}
seqValue :: Parse a -> Parse b -> Parse b
seqValue a b = a >>= \a' -> a' `seq` b

{-# RULES
"bind/rtrn"    forall f x. bind (rtrn x) f = f x
"shreTopLevel" forall x i. shreTopLevel i x = x
  #-}
  -- "bind/rtrn'let"   forall e x. let b = e in rtrn x = rtrn (let b = e in x)

infixr 0 -->
newtype a --> b = Func (Parse a -> Parse b)

instance (Sharing m) => Shareable m (a --> b) where
  shareArgs (Func f) = fmap Func (shareArgs f)

instance (Normalform Parse a1 a2, Normalform Parse b1 b2)
  => Normalform Parse (a1 --> b1) (a2 -> b2) where
    nf    mf =
      mf >> return (error "Plugin Error: Cannot capture function types")
    liftE mf = do
      f <- mf
      return (Func (liftE . fmap f . nf))

-- | Lift a unary function with the lifting scheme of the plugin.
liftParse1 :: (a -> b) -> Parse (a --> b)
liftParse1 f = rtrnFunc (\a -> a >>= \a' -> return (f a'))

-- | Lift a 2-ary function with the lifting scheme of the plugin.
liftParse2 :: (a -> b -> c) -> Parse (a --> b --> c)
liftParse2 f = rtrnFunc (\a  -> rtrnFunc (\b  ->
                a >>=  \a' -> b >>=     \b' -> return (f a' b')))

-- | Apply a lifted 2-ary function to its lifted arguments.
apply2 :: Parse (a --> b --> c) -> Parse a -> Parse b -> Parse c
apply2 f a b = app f a >>= \(Func f') -> f' b

-- | Apply a lifted 2-ary function to its arguments, where just the
-- first argument has to be lifted.
apply2Unlifted :: Parse (a --> b --> c)
               -> Parse a -> b -> Parse c
apply2Unlifted f a b = app f a >>= \(Func f') -> f' (return b)

-- | Apply a lifted 3-ary function to its lifted arguments.
apply3 :: Parse (a --> b --> c --> d)
       -> Parse a -> Parse b -> Parse c -> Parse d
apply3 f a b c = apply2 f a b >>= \(Func f') -> f' c
