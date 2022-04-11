{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE MagicHash              #-}
{-# OPTIONS_GHC -Wno-orphans        #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
{-|
Module      : Plugin.ParsePlugin.BuiltIn
Description : Built-In functions, types and type classes
Copyright   : (c) Kai-Oliver Prott (2020)
License     : BSD-3 Clause
Maintainer  : kai.prott@hotmail.de

This module contains lifted replacements for data types, functions
and type classes for Haskell's default Prelude.
This module is not supposed to be imported by users, please import
'Plugin.CurryPlugin.Prelude' instead.
-}
module Plugin.ParsePlugin.BuiltIn where

import qualified Prelude                as P
import           Prelude                     ( ($), Int, Integer, Char
                                             , Float, Double
                                             , Bool(..), Ordering(..) )
import qualified GHC.Real               as P
import qualified GHC.Base               as P
import           Unsafe.Coerce
import           GHC.Types                   ( RuntimeRep, Multiplicity )

import Plugin.ParsePlugin.Monad
import Plugin.Effect.Classes (Shareable(..))

-- | This is a lifted version of the unrestricted function type constructor
type (:->) r s a b = (Parse a -> Parse b)

-- | This is a lifted version of the restricted function type constructor
type (:-->) m r s a b = (Parse a -> Parse b)

-- | Alias for Shareable constraint specialized to the Parse monad.
type ShareableN a = Shareable Parse a

-- * Lifted list type and internal instances

-- | Lifted defintion for Haskell's default list type '[]'
data ListND a = Nil | Cons (Parse a) (Parse (ListND a))

-- | Lifted smart constructor for 'Nil'
nil :: Parse (ListND a)
nil = P.return Nil

-- | Lifted smart constructor for 'Cons'
cons :: Parse (a --> ListND a --> ListND a)
cons = rtrnFunc $ \a -> rtrnFunc $ \as -> P.return (Cons a as)

-- | Shareable instance for lists.
instance Shareable Parse a => Shareable Parse (ListND a) where
  shareArgs Nil         = P.return Nil
  shareArgs (Cons x xs) = Cons P.<$> share x P.<*> share xs

-- | Normalform instance for lists
instance Normalform Parse a1 a2 => Normalform Parse (ListND a1) [a2] where
  nf mxs = mxs P.>>= \case
    Nil       -> P.return []
    Cons x xs -> (:) P.<$> nf x P.<*> nf xs
  liftE mxs = mxs P.>>= \case
    []   -> P.return Nil
    x:xs -> Cons P.<$> P.return (liftE (P.return x))
                 P.<*> P.return (liftE (P.return xs))

-- * Lifted tuple types and internal instances

-- | Lifted defintion for Haskell's 2-ary tuple '(,)'
data Tuple2ND a b = Tuple2 (Parse a) (Parse b)

-- | Selector for the first component of a lifted 2-ary tuple
fst :: Parse (Tuple2ND a b --> a)
fst = rtrnFunc $ \t -> t P.>>= \(Tuple2 a _) -> a

-- | Selector for the second component of a lifted 2-ary tuple
snd :: Parse (Tuple2ND a b --> b)
snd = rtrnFunc $ \t -> t P.>>= \(Tuple2 _ b) -> b

-- | Shareable instance for 2-ary tuple
instance (Shareable Parse a, Shareable Parse b) =>
  Shareable Parse (Tuple2ND a b) where
    shareArgs (Tuple2 a b) = Tuple2 P.<$> share a P.<*> share b

-- | Normalform instance for 2-ary tuple
instance (Normalform Parse a1 a2, Normalform Parse b1 b2) =>
  Normalform Parse (Tuple2ND a1 b1) (a2, b2) where
    nf mxs = mxs P.>>= \(Tuple2 a b) -> (,) P.<$> nf a P.<*> nf b
    liftE mxs = mxs P.>>= \(a, b) -> Tuple2 P.<$> P.return (liftE (P.return a))
                                            P.<*> P.return (liftE (P.return b))

-- * Other lifted types and internal instances

-- | Lifted defintion for Haskell's 'String' type
type StringND = ListND P.Char

-- | Lifted defintion for Haskell's 'Ratio' type
data RatioND a = !(Parse a) :% !(Parse a)

-- | Shareable instance for Ratios
instance (Shareable Parse a) =>
  Shareable Parse (RatioND a) where
    shareArgs (a :% b) = (:%) P.<$> share a P.<*> share b

-- | Normalform instance for Ratios
instance (Normalform Parse a1 a2) =>
  Normalform Parse (RatioND a1) (P.Ratio a2) where
    nf mxs = mxs P.>>= \(a :% b) -> (P.:%) P.<$> nf a P.<*> nf b
    liftE mxs = mxs P.>>= \(a P.:% b) ->
      (:%) P.<$> P.return (liftE (P.return a))
           P.<*> P.return (liftE (P.return b))

-- | Lifted defintion for Haskell's 'Rational' type
type RationalND = RatioND Integer


-- * Lifted functions

-- $liftedFunctions
-- The pre-lifted functions are used to desugar
-- do-notation, (list) comprehensions or to replace their Haskell counterpart
-- in derived instances.

-- | Function to use for pattern match failures
-- Pattern match failure is translated to a failed for Curry,
-- ignoring the string.
pE :: ShareableN a => Parse (ListND Char --> a)
pE = rtrnFunc (P.>>= P.const P.empty)

-- | Lifted identity function
id :: Parse (a --> a)
id = rtrnFunc P.id

-- | Lifted logical negation
not :: Parse (Bool --> Bool)
not = liftParse1 P.not

otherwise :: Parse Bool 
otherwise = P.return True

-- Note: In order to be able to keep all type-applications
-- of the original code for the following "primops",
-- we introduce the same number and order of type variables,
-- even if they are unused

-- Lifted seq operator to force evaluation. Forces the effect and value.
seq :: forall (k :: RuntimeRep) a b. Parse (a --> b --> b)
seq = rtrnFunc $ \a -> rtrnFunc $ \b ->
  (a P.>>= \a' -> P.seq a' b)

-- Lifted function to desugar left sections.
leftSection :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (n :: Multiplicity) a b.
               Parse ((a --> b) --> a --> b)
leftSection = rtrnFunc $ \f -> rtrnFunc $ \a ->
  f `app` a

-- Lifted function to desugar right sections.
rightSection :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (r3 :: RuntimeRep)
                       (n1 :: Multiplicity) (n2 :: Multiplicity) a b c.
                Parse ((a --> b --> c) --> b --> a --> c)
rightSection = rtrnFunc $ \f -> rtrnFunc $ \b -> rtrnFunc $ \a ->
  f `app` a `app` b

-- | Lifted const function
const :: Parse (a --> b --> a)
const = rtrnFunc $ \a -> rtrnFunc $ \_ -> a

-- | Lifted logical and
(&&) :: Parse (Bool --> Bool --> Bool)
(&&) =  rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> a1 P.>>= \case
  False -> P.return False
  True  -> a2

-- | Lifted guard function used to desugar monad comprehensions
guard :: (AlternativeND f, forall x . ShareableN x => ShareableN (f x))
      => Parse (Bool --> f ())
guard = rtrnFunc $ \b -> b P.>>= \case
  True  -> pure `app` (P.return ())
  False -> empty

-- | Lifted append function for lists
append :: ShareableN a => Parse (ListND a --> ListND a --> ListND a)
append = rtrnFunc $ \xs -> rtrnFunc $ \ys -> xs P.>>= \case
  Nil       -> ys
  Cons a as -> P.return (Cons a (apply2 append as ys))

-- | Lifted concatMap function for lists
concatMap :: (ShareableN a, ShareableN b)
          => Parse ((a --> ListND b) --> ListND a --> ListND b)
concatMap = rtrnFunc $ \f -> rtrnFunc $ \xs -> xs P.>>= \case
  Nil       -> P.return Nil
  Cons a as -> append `app` (f `app` a) `app` (concatMap `app` f `app` as)

-- | Lifted map function for lists
map :: Parse ((a --> b) --> ListND a --> ListND b)
map = rtrnFunc $ \f' -> share f' P.>>= \f ->
  rtrnFunc $ \xs -> xs P.>>= \case
  Nil       -> P.return Nil
  Cons a as -> P.return (Cons (f `app` a) (apply2 map f as))

-- | Lifted coercion function to replace coercion in newtype-derived instances
-- We need to introduce this unused dummy k,
-- because we replace Data.Coerce.coerce (which has this k).
coerce :: forall (k :: RuntimeRep) a b. (ShareableN a, ShareableN b)
       => Parse (a --> b)
coerce = rtrnFunc $ \a -> a P.>>= \a' -> P.return (unsafeCoerce a')

-- | Lifted equality test for strings
eqString :: Parse (StringND --> StringND --> Bool)
eqString = (==)

(<#) :: Parse (Int --> Int --> Int)
(<#) = rtrnFunc $ \a -> rtrnFunc $ \b ->
  a P.>>= \ (P.I# a') -> b P.>>= \ (P.I# b') ->
   P.return (P.I# (a' P.<# b'))

(==#) :: Parse (Int --> Int --> Int)
(==#) = rtrnFunc $ \a -> rtrnFunc $ \b ->
  a P.>>= \ (P.I# a') -> b P.>>= \ (P.I# b') ->
  P.return (P.I# (a' P.==# b'))

-- |  Lifted composition operator for functions
(.) :: (ShareableN a, ShareableN b, ShareableN c)
    => Parse ((b --> c) --> (a --> b) --> a --> c)
(.) = rtrnFunc $ \f1 -> rtrnFunc $ \f2 -> rtrnFunc $ \a ->
  f1 `app` (f2 `app` a)

-- * Lifted Show type class, instances and functions

-- | Lifted ShowS type
type ShowSND = StringND --> StringND

-- | Lifted Show type class
class ShowND a where
  {-# MINIMAL showsPrec | show #-}
  showsPrec :: Parse (Int --> a --> ShowSND)
  showsPrec = rtrnFunc $ \_ -> rtrnFunc $ \x -> rtrnFunc $ \s ->
    apply2 append (show `app` x) s

  show :: Parse (a --> StringND)
  show = rtrnFunc $ \x -> apply2 shows x (P.return Nil)

  showList :: Parse (ListND a --> ShowSND)
  showList = rtrnFunc $ \ls -> rtrnFunc $ \s -> apply3 showsList__ shows ls s

showsList__ :: Parse ((a --> ShowSND) --> ListND a --> ShowSND)
showsList__ = rtrnFunc $ \showx -> rtrnFunc $ \list -> rtrnFunc $ \s ->
  list P.>>= \case
    Nil       -> apply2 append (liftE (P.return "[]")) s
    Cons x xs ->
      P.return (Cons (P.return '[') (apply2 showx x (apply3 showl showx xs s)))
  where
    showl = rtrnFunc $ \showx -> rtrnFunc $ \list -> rtrnFunc $ \s ->
      list P.>>= \case
        Nil       ->
          P.return (Cons (P.return ']') s)
        Cons y ys ->
          P.return (Cons (P.return ',')
            (apply2 showx y (apply3 showl showx ys s)))

shows :: ShowND a => Parse (a --> ShowSND)
shows = showsPrec `app` (P.return 0)

showString :: Parse (StringND --> ShowSND)
showString = append

showCommaSpace :: Parse ShowSND
showCommaSpace = showString `app` (liftE (P.return ", "))

showSpace :: Parse ShowSND
showSpace =  showString `app` (liftE (P.return " "))

showParen :: Parse (Bool --> ShowSND --> ShowSND)
showParen = rtrnFunc $ \b -> rtrnFunc $ \s -> b P.>>= \case
  True  -> apply2 (.) (showString `app` (liftE (P.return "(")))
          (apply2 (.) s (showString `app` (liftE (P.return ")"))))
  False -> s

instance ShowND Bool where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND () where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Int where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Integer where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Float where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Double where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)

instance ShowND Char where
  show = rtrnFunc $ \x -> liftE (P.show P.<$> x)
  showList = rtrnFunc $ \ls -> rtrnFunc $ \s ->
    liftE (P.showList P.<$> nf ls P.<*> nf s)

instance (ShowND a, ShareableN a) => ShowND (ListND a) where
  show = rtrnFunc $ \xs -> apply2 showList xs (P.return Nil)

-- * Lifted Eq type class, instances and functions

-- | Lifted Eq type class
class EqND a where
  (==) :: Parse (a --> a --> Bool)
  (==) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> not `app` (apply2 (/=) a1 a2)

  (/=) :: Parse (a --> a --> Bool)
  (/=) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> not `app` (apply2 (==) a1 a2)

instance EqND Bool where
  (==) = liftParse2 (P.==)
  (/=) = liftParse2 (P./=)

instance EqND () where
  (==) = rtrnFunc $ \_ -> rtrnFunc $ \_ -> P.return True
  (/=) = rtrnFunc $ \_ -> rtrnFunc $ \_ -> P.return False

instance EqND Int where
  (==) = liftParse2 (P.==)
  (/=) = liftParse2 (P./=)

instance EqND Integer where
  (==) = liftParse2 (P.==)
  (/=) = liftParse2 (P./=)

instance EqND Float where
  (==) = liftParse2 (P.==)
  (/=) = liftParse2 (P./=)

instance EqND Double where
  (==) = liftParse2 (P.==)
  (/=) = liftParse2 (P./=)

instance EqND Char where
  (==) = liftParse2 (P.==)
  (/=) = liftParse2 (P./=)

instance (EqND a, ShareableN a) => EqND (ListND a) where
  (==) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> a1 P.>>= \case
    Nil       -> a2 P.>>= \case
      Nil       -> P.return True
      Cons _ _  -> P.return False
    Cons x xs -> a2 P.>>= \case
      Nil       -> P.return False
      Cons y ys -> eqOn x y xs ys

instance (EqND a, EqND b, ShareableN a, ShareableN b) =>
  EqND (Tuple2ND a b) where
  (==) = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> do
    (Tuple2 a1 b1) <- x1
    (Tuple2 a2 b2) <- x2
    eqOn a1 a2 b1 b2

instance (EqND a, ShareableN a) => EqND (RatioND a) where
  (==) = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> do
    (a1 :% b1) <- x1
    (a2 :% b2) <- x2
    eqOn a1 a2 b1 b2

eqOn :: (EqND a1, EqND a2, ShareableN a1, ShareableN a2)
     => Parse a1 -> Parse a1 -> Parse a2 -> Parse a2 -> Parse Bool
eqOn x y xs ys = apply2 (&&) (apply2 (==) x y) (apply2 (==) xs ys)

-- * Lifted Ord type class, instances and functions

-- | Lifted Ord type class
class EqND a => OrdND a where
  {-# MINIMAL compare | (<=) #-}
  compare :: Parse (a --> a --> Ordering)
  compare = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 (==) a1 a2 P.>>= \b1 -> if b1
      then P.return EQ
      else apply2 (<=) a1 a2 P.>>= \b2 -> if b2
        then P.return LT
        else P.return GT

  (<) :: Parse (a --> a --> Bool)
  (<) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      LT -> P.return True
      _  -> P.return False

  (<=) :: Parse (a --> a --> Bool)
  (<=) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      GT -> P.return False
      _  -> P.return True

  (>) :: Parse (a --> a --> Bool)
  (>) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      GT -> P.return True
      _  -> P.return False

  (>=) :: Parse (a --> a --> Bool)
  (>=) = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
    apply2 compare a1 a2 P.>>= \case
      LT -> P.return False
      _  -> P.return True

  -- This default implementation is replaced at compile-time with maxDefault
  max :: Parse (a --> a --> a)
  max = P.undefined

  -- This default implementation is replaced at compile-time with minDefault
  min :: Parse (a --> a --> a)
  min = P.undefined

maxDefault :: (OrdND a, ShareableN a) => Parse (a --> a --> a)
maxDefault = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
  share a1 P.>>= \a1' -> share a2 P.>>= \a2' ->
  apply2 (>=) a1' a2' P.>>= \case
    True -> a1'
    _    -> a2'

minDefault :: (OrdND a, ShareableN a) => Parse (a --> a --> a)
minDefault = rtrnFunc $ \a1 -> rtrnFunc $ \a2 ->
  share a1 P.>>= \a1' -> share a2 P.>>= \a2' ->
  apply2 (<=) a1' a2' P.>>= \case
    True -> a1'
    _    -> a2'

instance OrdND Bool where
  compare = liftParse2 P.compare

instance OrdND () where
  compare = rtrnFunc $ \_ -> rtrnFunc $ \_ -> P.return EQ

instance OrdND Int where
  compare = liftParse2 P.compare

instance OrdND Integer where
  compare = liftParse2 P.compare

instance OrdND Float where
  compare = liftParse2 P.compare

instance OrdND Double where
  compare = liftParse2 P.compare

instance (OrdND a, ShareableN a) => OrdND (ListND a) where
  compare = rtrnFunc $ \x -> rtrnFunc $ \y ->
    x P.>>= \x' -> y P.>>= \y' -> case (x', y') of
      (Nil      , Nil      ) -> P.return EQ
      (Nil      , Cons _ _ ) -> P.return LT
      (Cons _ _ , Nil      ) -> P.return GT
      (Cons a as, Cons b bs) -> apply2 compare a b P.>>= \case
        EQ -> apply2 compare as bs
        o  -> P.return o

instance (OrdND a, OrdND b, ShareableN a, ShareableN b) =>
  OrdND (Tuple2ND a b) where
  compare = rtrnFunc $ \x -> rtrnFunc $ \y ->
    x P.>>= \x' -> y P.>>= \y' -> case (x', y') of
      (Tuple2 a1 b1, Tuple2 a2 b2) -> apply2 compare a1 a2 P.>>= \case
        EQ -> apply2 compare b1 b2
        o  -> P.return o

-- * Lifted Num type class, instances and functions

-- | Lifted Num type class
class NumND a where
  (+) :: Parse (a --> a --> a)
  (-) :: Parse (a --> a --> a)
  (-) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    (+) `app` a `app` (negate `app` b)
  (*) :: Parse (a --> a --> a)
  negate :: Parse (a --> a)
  negate = rtrnFunc $ \a -> (-) `app` (fromInteger `app` (P.return 0)) `app` a
  abs    :: Parse (a --> a)
  signum :: Parse (a --> a)
  fromInteger :: Parse (P.Integer --> a)

instance NumND Int where
  (+) = liftParse2 (P.+)
  (-) = liftParse2 (P.-)
  (*) = liftParse2 (P.*)
  negate = liftParse1 P.negate
  abs    = liftParse1 P.abs
  signum = liftParse1 P.signum
  fromInteger = liftParse1 P.fromInteger

instance NumND Integer where
  (+) = liftParse2 (P.+)
  (-) = liftParse2 (P.-)
  (*) = liftParse2 (P.*)
  negate = liftParse1 P.negate
  abs    = liftParse1 P.abs
  signum = liftParse1 P.signum
  fromInteger = liftParse1 P.fromInteger

instance NumND Float where
  (+) = liftParse2 (P.+)
  (-) = liftParse2 (P.-)
  (*) = liftParse2 (P.*)
  negate = liftParse1 P.negate
  abs    = liftParse1 P.abs
  signum = liftParse1 P.signum
  fromInteger = liftParse1 P.fromInteger

instance NumND Double where
  (+) = liftParse2 (P.+)
  (-) = liftParse2 (P.-)
  (*) = liftParse2 (P.*)
  negate = liftParse1 P.negate
  abs    = liftParse1 P.abs
  signum = liftParse1 P.signum
  fromInteger = liftParse1 P.fromInteger

-- * Lifted Fractional type class, instances and functions

-- | Lifted Fractional type class
class NumND a => FractionalND a where
  {-# MINIMAL fromRational, (recip | (/)) #-}

  (/) :: Parse (a --> a --> a)
  (/) = rtrnFunc $ \x -> rtrnFunc $ \y -> apply2 (*) x  (recip `app` y)

  recip :: Parse (a --> a)
  recip = rtrnFunc $ \x -> apply2 (/) (fromInteger `app` (P.return 1)) x

  fromRational :: Parse (RationalND --> a)

instance FractionalND Float where
  (/) = liftParse2 (P./)
  fromRational = rtrnFunc $ \r -> P.fromRational P.<$> nf r

instance FractionalND Double where
  (/) = liftParse2 (P./)
  fromRational = rtrnFunc $ \r -> P.fromRational P.<$> nf r

-- * Lifted Real type class, instances and functions

-- | Lifted Real type class
class (NumND a, OrdND a) => RealND a where
  toRational :: Parse (a --> RationalND)

instance RealND Int where
  toRational = rtrnFunc $ \i -> P.return ((toInteger `app` i) :% (P.return 1))

instance RealND Integer where
  toRational = rtrnFunc $ \i -> P.return (i :% (P.return 1))

instance RealND Float where
  toRational = rtrnFunc $ \f -> liftE (P.toRational P.<$> f)

instance RealND Double where
  toRational = rtrnFunc $ \d -> liftE (P.toRational P.<$> d)

-- * Lifted Integral type class, instances and functions

-- | Lifted Integral type class
class (RealND a, EnumND a) => IntegralND a where
  quot      :: Parse (a --> a --> a)
  rem       :: Parse (a --> a --> a)
  div       :: Parse (a --> a --> a)
  mod       :: Parse (a --> a --> a)
  quotRem   :: Parse (a --> a --> Tuple2ND a a)
  divMod    :: Parse (a --> a --> Tuple2ND a a)
  toInteger :: Parse (a --> Integer)

  quot   = rtrnFunc $ \n -> rtrnFunc $ \d -> fst `app` (apply2 quotRem n d)
  rem    = rtrnFunc $ \n -> rtrnFunc $ \d -> snd `app` (apply2 quotRem n d)
  div    = rtrnFunc $ \n -> rtrnFunc $ \d -> fst `app` (apply2 divMod n d)
  mod    = rtrnFunc $ \n -> rtrnFunc $ \d -> snd `app` (apply2 divMod n d)

  -- This default implementation is replaced at compile-time with divModDefault
  divMod = P.undefined

divModDefault :: (IntegralND a, ShareableN a)
              => Parse (a --> a --> Tuple2ND a a)
divModDefault = rtrnFunc $ \n' -> rtrnFunc $ \d' ->
  share n' P.>>= \n -> share d' P.>>= \d ->
  let qr' = apply2 quotRem n d
  in share qr' P.>>= \qr ->
     qr P.>>= \(Tuple2 q r) -> apply2 (==) (signum `app` r)
                                           (negate `app` (signum `app` d))
        P.>>= \b -> if b
          then P.return (Tuple2 (apply2 (-) q
                                   (fromInteger `app` (P.return 1)))
                                   (apply2 (+) r d))
          else qr

instance IntegralND Int where
  quot = liftParse2 (P.quot)
  rem  = liftParse2 (P.rem)
  div  = liftParse2 (P.div)
  mod  = liftParse2 (P.mod)

  quotRem = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.quotRem P.<$> a1 P.<*> a2)
  divMod = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.divMod P.<$> a1 P.<*> a2)

  toInteger = liftParse1 (P.toInteger)

instance IntegralND Integer where
  quot = liftParse2 (P.quot)
  rem  = liftParse2 (P.rem)
  div  = liftParse2 (P.div)
  mod  = liftParse2 (P.mod)

  quotRem = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.quotRem P.<$> a1 P.<*> a2)
  divMod = rtrnFunc $ \a1 -> rtrnFunc $ \a2 -> liftE
    (P.divMod P.<$> a1 P.<*> a2)

  toInteger = rtrnFunc P.id

-- * Lifted Monad & Co type classes and instances

infixl 1 >>=, >>
infixl 4 <$, <*, *>, <*>
-- | Lifted Functor type class
class FunctorND f where
  fmap :: (ShareableN a, ShareableN b) => Parse ((a --> b) --> f a --> f b)
  (<$) :: (ShareableN a, ShareableN b) => Parse (a --> f b --> f a)
  (<$) = rtrnFunc $ \a -> rtrnFunc $ \f ->
    apply2 fmap (const `app` a) f

instance FunctorND (Tuple2ND a) where
  fmap = rtrnFunc $ \f -> rtrnFunc $ \t -> t P.>>= \case
    Tuple2 a b -> P.return (Tuple2 a (f `app` b))

instance FunctorND ListND where
  fmap = rtrnFunc $ \f -> rtrnFunc $ \l -> l P.>>= \case
    Nil       -> P.return Nil
    Cons x xs -> P.return (Cons (f `app` x) (apply2 fmap f xs))

-- | Lifted Applicative type class
class FunctorND f => ApplicativeND f where
  pure :: ShareableN a => Parse (a --> f a)

  (<*>) :: (ShareableN a, ShareableN b) => Parse (f (a --> b) --> f a --> f b)
  (<*>) = rtrnFunc $ \f -> rtrnFunc $ \a ->
    apply3 liftA2 (liftParse1 P.id) f a

  liftA2 :: (ShareableN a, ShareableN b, ShareableN c)
         => Parse ((a --> b --> c) --> f a --> f b --> f c)
  liftA2 = rtrnFunc $ \f -> rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply2 (<*>) (apply2 fmap f a) b

  (*>) :: (ShareableN a, ShareableN b) => Parse (f a --> f b --> f b)
  (*>) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply3 liftA2 (liftParse2 (P.flip P.const)) a b

  (<*) :: (ShareableN a, ShareableN b) => Parse (f a --> f b --> f a)
  (<*) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply3 liftA2 const a b
  {-# MINIMAL pure, ((<*>) | liftA2) #-}

instance ApplicativeND ListND where
  pure = rtrnFunc $ \a -> P.return (Cons a (P.return Nil))
  (<*>) = rtrnFunc $ \fs -> rtrnFunc $ \as ->
    apply2 concatMap (rtrnFunc $ \a ->
    apply2 fmap      (rtrnFunc $ \f -> f `app` a) fs) as

  -- | Lifted Alternative type class
class ApplicativeND f => AlternativeND f where
  empty :: ShareableN a => Parse (f a)
  (<|>) :: ShareableN a => Parse (f a --> f a --> f a)
  some  :: ShareableN a => Parse (f a --> f (ListND a))
  some = rtrnFunc $ \v ->
    let many_v = apply2 (<|>) some_v (pure `app` (P.return Nil))
        some_v = apply3 liftA2 cons v many_v
    in some_v
  many  :: ShareableN a => Parse (f a --> f (ListND a))
  many = rtrnFunc $ \v ->
    let many_v = apply2 (<|>) some_v (pure `app` (P.return Nil))
        some_v = apply3 liftA2 cons v many_v
    in many_v

instance AlternativeND ListND where
  empty = nil
  (<|>) = append

-- | Lifted Monad type class
class ApplicativeND m => MonadND m where
  (>>=) :: (ShareableN a, ShareableN b) => Parse (m a --> (a --> m b) --> m b)
  (>>)  :: (ShareableN a, ShareableN b) => Parse (m a --> m b --> m b)
  (>>) = rtrnFunc $ \a -> rtrnFunc $ \b ->
    apply2 (>>=) a (rtrnFunc (P.const b))
  return :: ShareableN a => Parse (a --> m a)
  return = pure
  {-# MINIMAL (>>=) #-}

instance MonadND ListND where
  (>>=) = rtrnFunc $ \a -> rtrnFunc $ \f -> a P.>>= \case
    Nil       -> P.return Nil
    Cons x xs -> apply2 append (f `app` x) (apply2 (>>=) xs f)

-- | Lifted MonadFail type class
class MonadND m => MonadFailND m where
  fail :: ShareableN a => Parse (StringND --> m a)

instance MonadFailND ListND where
  fail = rtrnFunc $ \_ -> P.return Nil

-- * Lifted Enum type class, instances and functions

-- | Lifted Enum type class
class EnumND a where
  succ :: Parse (a --> a)
  succ = rtrnFunc $ \a ->
    toEnum `app` (apply2 (+) (P.return 1) (fromEnum `app` a))
  pred :: Parse (a --> a)
  pred = rtrnFunc $ \a ->
    toEnum `app` (apply2 (-) (P.return 1) (fromEnum `app` a))

  toEnum   :: Parse (Int --> a)
  fromEnum :: Parse (a --> Int)

  enumFrom       :: Parse (a             --> ListND a)
  enumFrom       = rtrnFunc $ \x1 ->
    apply2 map toEnum (enumFrom `app`
      (fromEnum `app` x1))

  enumFromThen   :: Parse (a --> a       --> ListND a)
  enumFromThen   = rtrnFunc $ \x1 -> rtrnFunc $ \x2 ->
    apply2 map toEnum (apply2 enumFromThen
      (fromEnum `app` x1) (fromEnum `app` x2))

  enumFromTo     :: Parse (a       --> a --> ListND a)
  enumFromTo     = rtrnFunc $ \x1 ->                   rtrnFunc $ \x3 ->
    apply2 map toEnum (apply2 enumFromTo
      (fromEnum `app` x1)                      (fromEnum `app` x3))

  enumFromThenTo :: Parse (a --> a --> a --> ListND a)
  enumFromThenTo = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> rtrnFunc $ \x3 ->
    apply2 map toEnum (apply3 enumFromThenTo
      (fromEnum `app` x1) (fromEnum `app` x2) (fromEnum `app` x3))

instance EnumND Int where
  succ = (+) `app` (P.return 1)
  pred = (-) `app` (P.return 1)

  toEnum   = id
  fromEnum = id

  enumFrom = rtrnFunc $ \x1 ->
    x1 P.>>= \v1 ->
    liftE (P.return (P.enumFrom v1))

  enumFromThen = rtrnFunc $ \x1 -> rtrnFunc $ \x2 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 ->
    liftE (P.return (P.enumFromThen v1 v2))

  enumFromTo = rtrnFunc $ \x1 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromTo v1 v3))

  enumFromThenTo = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromThenTo v1 v2 v3))

instance EnumND Integer where
  succ = (+) `app` (P.return 1)
  pred = (-) `app` (P.return 1)

  toEnum   = toInteger
  fromEnum = fromInteger

  enumFrom = rtrnFunc $ \x1 ->
    x1 P.>>= \v1 ->
    liftE (P.return (P.enumFrom v1))

  enumFromThen = rtrnFunc $ \x1 -> rtrnFunc $ \x2 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 ->
    liftE (P.return (P.enumFromThen v1 v2))

  enumFromTo = rtrnFunc $ \x1 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromTo v1 v3))

  enumFromThenTo = rtrnFunc $ \x1 -> rtrnFunc $ \x2 -> rtrnFunc $ \x3 ->
    x1 P.>>= \v1 -> x2 P.>>= \v2 -> x3 P.>>= \v3 ->
    liftE (P.return (P.enumFromThenTo v1 v2 v3))

-- * Lifted Bounded type class, instances and functions

-- | Lifted Bounded type class
class BoundedND a where
  minBound :: Parse a
  maxBound :: Parse a

instance BoundedND Int where
  minBound = P.return P.minBound
  maxBound = P.return P.maxBound

class IsStringND a where
  fromString :: Parse (StringND --> a)

instance (a ~ Char) => IsStringND (ListND a) where
  fromString = rtrnFunc $ \x -> x

{-

class Fractional a => Floating a where
  pi                  :: a
  exp, log, sqrt      :: a -> a
  (**), logBase       :: a -> a -> a
  sin, cos, tan       :: a -> a
  asin, acos, atan    :: a -> a
  sinh, cosh, tanh    :: a -> a
  asinh, acosh, atanh :: a -> a

  log1p               :: a -> a
  expm1               :: a -> a
  log1pexp            :: a -> a
  log1mexp            :: a -> a

  x ** y              =  exp (log x * y)
  logBase x y         =  log y / log x
  sqrt x              =  x ** 0.5
  tan  x              =  sin  x / cos  x
  tanh x              =  sinh x / cosh x

  log1p x = log (1 + x)
  expm1 x = exp x - 1
  log1pexp x = log1p (exp x)
  log1mexp x = log1p (negate (exp x))
-}
