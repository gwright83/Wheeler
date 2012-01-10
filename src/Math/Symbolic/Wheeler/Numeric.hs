--
-- Numeric.hs
--
-- The Numeric datatype; the base module imported by anything
-- that operates on symbolic numbers. 
--
-- Gregory Wright, 3 June 2011.
--
-- Only integers and rationals for now.  Should extend to
-- floating and complex (integer, rational and floating) in the
-- future.
--
-- Copyright (c) 2011, Antiope Associates, all rights reserved.
--

module Math.Symbolic.Wheeler.Numeric (
       Numeric (..)
) where


import Data.Ratio

import Math.Symbolic.Wheeler.Precedence


data Numeric = I Integer           -- A numeric integer
             | Q Integer Integer   -- A numeric rational


instance Eq Numeric where
    (==) (I n)   (I n')    = n == n'
    (==) (I n)   (Q n' d)  = n  * d  == n'
    (==) (Q n d) (I n')    = n' * d  == n
    (==) (Q n d) (Q n' d') = n  * d' == n' * d 


instance Ord Numeric where
    compare (I n) (I n')      = compare n n'
    compare (I n) (Q n' d)    = compare (n % 1) (n' % d)
    compare (Q n d) (I n')    = compare (n % d) (n' % 1)
    compare (Q n d) (Q n' d') = compare (n % d) (n' % d') 


instance Show Numeric where
    showsPrec d (I n)         = showParen (d > applicationPrecedence) $ shows n
    showsPrec d (Q num denom) = showParen (d > applicationPrecedence) $ shows num      .
                                                                        showString "/" .
                                                                        shows denom


instance Num Numeric where
    fromInteger n = I n
    (+) (I n)   (I n')    = I (n + n')
    (+) (Q n d) (Q n' d') = Q (d' * n + n' * d) (d * d')
    (+) (Q n d) (I n')    = Q (n + n' * d) d
    (+) (I n)   (Q n' d)  = Q (n * d + n') d
    (-) (I n)   (I n')    = I (n - n')
    (-) (Q n d) (Q n' d') = Q (d' * n - n' * d) (d * d')
    (-) (Q n d) (I n')    = Q (n - n' * d) d
    (-) (I n)   (Q n' d)  = Q (n * d - n') d
    (*) (I n)   (I n')    = I (n * n')
    (*) (Q n d) (Q n' d') = Q (n * n') (d * d')
    (*) (Q n d) (I n')    = Q (n * n') d
    (*) (I n)   (Q n' d)  = Q (n * n') d
    abs (I n)      = if n < 0 then I (-n) else I n
    abs (Q n d)    = if (n < 0 && d > 0) || (n > 0 && d < 0) then Q (-n) d else Q n d
    signum (I n)   = if n < 0 then I (-1) else I 1
    signum (Q n d) = if (n < 0 && d > 0) || (n > 0 && d < 0) then I (-1) else I 1


instance Fractional Numeric where
    fromRational r = Q (numerator r) (denominator r)
    recip (I n)    = Q 1 n
    recip (Q n d)  = Q d n
         

instance Enum Numeric where
    succ x           = x + 1
    pred x           = x - 1
    toEnum           = fromIntegral
    fromEnum (I n)   = fromIntegral n
    fromEnum (Q n d) = fromIntegral $ n `quot` d 

