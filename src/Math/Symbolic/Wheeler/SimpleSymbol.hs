--
-- SimpleSymbol.hs
--
-- Simple symbols.
--
-- Gregory Wright, 30 August 2011
--

module Math.Symbolic.Wheeler.SimpleSymbol where

import System.IO.Unsafe

import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Complexity
import Math.Symbolic.Wheeler.Named
import {-# SOURCE #-} Math.Symbolic.Wheeler.Symbol()
import Math.Symbolic.Wheeler.UniqueID


data S = S {
     simpleIdentifier    :: IO Id,
     simpleName          :: String,
     simpleTeXName       :: String,
     simpleComplexity    :: Complexity,
     simpleCommutativity :: Commutativity
}

instance Eq S where
    (==) x y = unsafePerformIO $ do 
        x' <- simpleIdentifier x
        y' <- simpleIdentifier y
        return (x' == y')

instance Ord S where
    compare x y = if isNonCommuting x && isNonCommuting y
                     then if repSpace x == repSpace y
                              then GT
                              else compare (simpleName x) (simpleName y)
                     else compare (simpleName x) (simpleName y)

instance Show S where
     showsPrec _ s = showString (name s)

instance Named S where
    name    = simpleName
    teXName = simpleTeXName

instance Identified S where
    identifier s = unsafePerformIO $ do s' <- simpleIdentifier s; return s'

instance Commutable S where
    commutativity = simpleCommutativity

