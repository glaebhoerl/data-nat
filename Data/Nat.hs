{-# LANGUAGE CPP #-}

#ifdef HAVE_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#ifdef HAVE_GENERIC
{-# LANGUAGE DeriveGeneric #-}
#endif

{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-name-shadowing #-}

-- | Module:      Data.Nat
--   License:     BSD3
--   Copyright:   2012 Gábor Lehel
--   Stability:   experimental
--   Maintainer:  Gábor Lehel <illissius@gmail.com>
--   Portability: portable
--
-- Operations which are undefined mathematically (@0 / 0@, @infinity * 0@, @infinity - infinity@, etc.)
-- also have undefined results in this implementation.

module Data.Nat (Nat(..), nat, foldNat, unfoldNat, infinity, diff) where

import Control.Arrow            (first)
import Data.Ix                  (Ix(..))
import Numeric.Natural.Internal (Whole(..))

#ifdef HAVE_TYPEABLE
import Data.Typeable            (Typeable)
#define MAYBE_TYPEABLE , Typeable
#else
#define MAYBE_TYPEABLE
#endif

#ifdef HAVE_GENERIC
import GHC.Generics             (Generic)
#define MAYBE_GENERIC  , Generic
#else
#define MAYBE_GENERIC
#endif

data Nat = Zero | Succ Nat
           deriving (Eq, Ord, Read, Show MAYBE_TYPEABLE MAYBE_GENERIC)

negative :: String -> a
negative f = error ("Data.Nat." ++ f ++ ": would be negative")

instance Enum Nat where
    succ                   = Succ
    pred                   = nat (negative "pred") id
    toEnum                 = fromInteger  . fromIntegral
    fromEnum               = fromIntegral . toInteger
    enumFrom               = iterate succ
    enumFromTo     f    to = enumFromThenTo f (succ f) to
    enumFromThen   f th    = enumFromThenTo f th (if f < th then maxBound else minBound)
    enumFromThenTo f th to = either (const []) (accum f) (diff to f `divD` diff_th_f) where
        accum n            = nat [n] ((n:) . accum (step n))
        step               = either subtract (+) diff_th_f
        diff_th_f          = diff th f
        divD               = either (\x -> either (Right                            . div x) (Left  . div x))
                                    (\x -> either ((if x == 0 then Right else Left) . div x) (Right . div x))
                                    -- maybe this could be done simpler?

instance Bounded Nat where
    minBound = 0
    maxBound = infinity -- is this polite?

instance Num Nat where
    (+) n       = foldNat n succ
    (*) n       = foldNat 0 (+n)
    (-) n       = nat n ((-) $! nat (negative "-") id n)
    negate      = nat 0 (const $ negative "negate")
    abs         = id
    signum      = nat 0 (const 1)
    fromInteger = unfoldNat $ \n -> case n of n | n < 0     -> negative "fromInteger"
                                                | n > 0     -> Just (n - 1)
                                                | otherwise -> Nothing

instance Real Nat where
    toRational = toRational . toNatural

instance Integral Nat where
    quotRem n m = either (const (0, n)) (\x -> first succ (quotRem x m)) (diff n m)
    divMod      = quotRem
    toInteger   = toInteger . toNatural

instance Whole Nat where
    toNatural  = foldNat 0 succ
    unsafePred = pred

instance Ix Nat where
    range     (n, m)   = [n..m]
    index     (n, m) i = fromIntegral (n - i)
    inRange   (n, m) i = n <= i && i <= m
    rangeSize (n, m)   = fromIntegral (m - n)

-- | Shallow deconstruction. Returns the first argument if @Zero@, applies the second argument to the inner value if @Succ@.
nat :: r -> (Nat -> r) -> Nat -> r
nat z s Zero     = z
nat z s (Succ n) = s n

-- | Returns the first argument if @Zero@, applies the second argument recursively for each @Succ@.
foldNat :: r -> (r -> r) -> Nat -> r
foldNat z s = nat z (s . foldNat z s)

-- | Build a @Nat@ from a seed value: the first argument should return the next seed value
--   if the building is to continue, or @Nothing@ if it is to stop.  A @Succ@ is added at each iteration.
unfoldNat :: (a -> Maybe a) -> a -> Nat
unfoldNat f a = maybe Zero (succ . unfoldNat f) (f a)

-- | Very big!
infinity :: Nat
infinity = succ infinity

-- | > diff n m | n >= m    = Right (n - m)
--   >          | otherwise = Left  (m - n)
diff :: Nat -> Nat -> Either Nat Nat
diff (Succ n) (Succ m) = diff n m
diff n        Zero     = Right n
diff Zero     n        = Left  n
