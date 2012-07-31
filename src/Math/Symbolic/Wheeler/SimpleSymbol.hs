--
-- SimpleSymbol.hs
--
-- Simple symbols.
--
-- Gregory Wright, 30 August 2011
--

module Math.Symbolic.Wheeler.SimpleSymbol where

import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Complexity
import Math.Symbolic.Wheeler.Named
import {-# SOURCE #-} Math.Symbolic.Wheeler.Symbol()
import Math.Symbolic.Wheeler.UniqueID


data S = S {
     simpleIdentifier    :: Id,
     simpleName          :: String,
     simpleTeXName       :: String,
     simpleType          :: SymbolType,
     simpleComplexity    :: Complexity,
     simpleCommutativity :: Commutativity
}

instance Eq S where
    (==) x y = if (simpleType x == Pattern && simpleType y /= Pattern) ||
                  (simpleType x /= Pattern && simpleType y == Pattern)
                  then True
                  else ((simpleIdentifier x) == (simpleIdentifier y)) &&
                       ((simpleType x)       == (simpleType y))


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
    identifier = simpleIdentifier

instance Commutable S where
    commutativity = simpleCommutativity

