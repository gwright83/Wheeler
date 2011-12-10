--
-- Indexed.hs
--
-- General indexed quantities.
--
-- Gregory Wright, 27 August 2011
--

module Math.Symbolic.Wheeler.Indexed where


import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Complexity
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.UniqueID


data I = I {
     indexedIdentifier    :: Id,
     indexedName          :: String,
     indexedTeXName       :: String,
     indexedIndices       :: [ Index ],
     indexedComplexity    :: Complexity,
     indexedCommutativity :: Commutativity
}

data Index = Index Expr

instance Eq I where
    (==) x y = (indexedIdentifier x) == (indexedIdentifier y)

instance Ord I where
    compare _ _ = GT

instance Named I where
    name    = indexedName
    teXName = indexedTeXName

instance Identified I where
    identifier = indexedIdentifier

instance Show I where
    showsPrec _ i = showString (indexedName i)

instance Commutable I where
    commutativity = indexedCommutativity
