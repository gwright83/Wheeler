--
-- Symbol.hs
--
-- Top level module for symbolic objects: Simple and
-- Indexed symbols.
--
-- Gregory Wright, 26 August 2011
--

module Math.Symbolic.Wheeler.Symbol where


import Data.Maybe
import System.IO.Unsafe

import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Complexity
import {-# SOURCE #-} Math.Symbolic.Wheeler.DiracSpinor
import {-# SOURCE #-} Math.Symbolic.Wheeler.Indexed
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.SimpleSymbol
import Math.Symbolic.Wheeler.SumOrd
import {-# SOURCE #-} Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.UniqueID


data Symbol = Simple S
            | Indexed I
            | Tensor T
            | DiracSpinor D

instance Named Symbol where
    name (Simple s)         = name s
    name (Indexed i)        = name i
    name (Tensor t)         = name t
    name (DiracSpinor d)    = name d
    teXName (Simple s)      = teXName s
    teXName (Indexed i)     = teXName i
    teXName (Tensor t)      = teXName t
    teXName (DiracSpinor d) = teXName d

instance Identified Symbol where
    identifier (Simple s)      = identifier s
    identifier (Indexed i)     = identifier i
    identifier (Tensor t)      = identifier t
    identifier (DiracSpinor d) = identifier d

instance Eq Symbol where
    (==) (Simple x)  (Simple y)           = x == y
    (==) (Simple _)  _                    = False
    (==) (Indexed x) (Indexed y)          = x == y
    (==) (Indexed _) _                    = False
    (==) (Tensor x)  (Tensor y)           = x == y
    (==) (Tensor _)           _           = False
    (==) (DiracSpinor x)  (DiracSpinor y) = x == y
    (==) (DiracSpinor _)  _               = False

instance Show Symbol where
    showsPrec d (Simple s)      = showsPrec d s
    showsPrec d (Indexed i)     = showsPrec d i
    showsPrec d (Tensor t)      = showsPrec d t
    showsPrec d (DiracSpinor s) = showsPrec d s

instance Commutable Symbol where
    commutativity (Simple s)      = commutativity s
    commutativity (Indexed i)     = commutativity i
    commutativity (Tensor t)      = commutativity t
    commutativity (DiracSpinor d) = commutativity d


-- If both symbols are non-commuting, compare just
-- returns GT, which will prevent reordering of the
-- factors.

instance Ord Symbol where
    compare (Simple x)  (Simple y)           = compare x y
    compare (Simple _)  (Indexed _)          = LT
    compare (Simple _)  (Tensor _)           = LT
    compare (Simple _)  (DiracSpinor _)      = LT
    compare (Indexed _) (Simple _)           = GT
    compare (Indexed x) (Indexed y)          = compare x y
    compare (Indexed x) (Tensor y)           = compare (name x) (name y)
    compare (Indexed x) (DiracSpinor y)      = compare (name x) (name y)
    compare (Tensor _)  (Simple _)           = GT
    compare (Tensor x)  (Indexed y)          = compare (name x) (name y)
    compare (Tensor x)  (Tensor y)           = compare x y
    compare (Tensor _)  (DiracSpinor _)      = GT
    compare (DiracSpinor _) (Simple _)       = GT
    compare (DiracSpinor x) (Indexed y)      = compare (name x) (name y)
    compare (DiracSpinor _) (Tensor _)       = GT
    compare (DiracSpinor x) (DiracSpinor y)  = compare x y

instance SumOrd Symbol where
    sumCompare (Simple x) (Simple y)           = compare (name x) (name y)
    sumCompare (Simple _) _                    = GT
    sumCompare (Indexed x) (Indexed y)         = compare (name x) (name y)
    sumCompare (Indexed _) _                   = GT
    sumCompare (Tensor x) (Tensor y)           = compare (name x) (name y)
    sumCompare (Tensor _) _                    = GT
    sumCompare (DiracSpinor x) (DiracSpinor y) = compare (name x) (name y)
    sumCompare (DiracSpinor _) _               = GT

-- Functions to check the type of a Symbol:

isSimple :: Symbol -> Bool
isSimple (Simple _) = True
isSimple _          = False

isIndexed :: Symbol -> Bool
isIndexed (Indexed _) = True
isIndexed _           = False

isTensor :: Symbol -> Bool
isTensor (Tensor _) = True
isTensor _          = False

isDiracSpinor :: Symbol -> Bool
isDiracSpinor (DiracSpinor _) = True
isDiracSpinor _               = False


-- A few handy functions for defining symbols:

simpleSymbol :: String -> Symbol
simpleSymbol s = unsafePerformIO $ do
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = s
                                       , simpleTeXName       = s
                                       , simpleType          = Regular
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = Commuting
                                       }

ncSymbol :: String -> String -> Symbol
ncSymbol s rep = unsafePerformIO $ do 
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = s
                                       , simpleTeXName       = s
                                       , simpleType          = Regular
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = NonCommuting (RepSpace rep)
                                       }
                       
mkPatternSymbol :: String -> Symbol
mkPatternSymbol s = unsafePerformIO $ do 
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = s
                                       , simpleTeXName       = s
                                       , simpleType          = Pattern
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = Commuting
                                       }

mkNcPatternSymbol :: String -> String -> Symbol
mkNcPatternSymbol s rep = unsafePerformIO $ do 
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = s
                                       , simpleTeXName       = s
                                       , simpleType          = Pattern
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = NonCommuting (RepSpace rep)
                                       }


mkPlaceholder :: Symbol
mkPlaceholder = unsafePerformIO $ do
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = "PHLD"
                                       , simpleTeXName       = "PHLD"
                                       , simpleType          = Placeholder
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = Commuting
                                       }


mkNcPlaceholder :: String -> Symbol
mkNcPlaceholder rep = unsafePerformIO $ do 
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = "PHLD"
                                       , simpleTeXName       = "PHLD"
                                       , simpleType          = Placeholder
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = NonCommuting (RepSpace rep)
                                       }


-- Show the internal details of a Symbol:

showsSymbol_ :: Symbol -> ShowS
showsSymbol_ (Simple s)      = if simpleCommutativity s == Commuting
                                   then showString "$ simpleSymbol \"" .
                                        showString (simpleName s) .
                                        showString "\"" 
                                   else showString "$ ncSymbol \"" .
                                        showString (simpleName s)  .
                                        showString "\" \""         .
                                        showString (fromJust $ repSpace s)    .
                                        showString "\""
showsSymbol_ (Indexed i)     = showString "Indexed " .
                               shows (identifier i)
showsSymbol_ (Tensor t)      = showString "Tensor "  .
                               shows (identifier t)
showsSymbol_ (DiracSpinor d) = showString "DiracSpinor "  .
                               shows (identifier d)