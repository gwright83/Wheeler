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

import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Complexity
import {-# SOURCE #-} Math.Symbolic.Wheeler.Indexed
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.SimpleSymbol
import Math.Symbolic.Wheeler.SumOrd
import {-# SOURCE #-} Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.UniqueID


data Symbol = Simple S
            | Indexed I
            | Tensor T

instance Named Symbol where
    name (Simple s)     = name s
    name (Indexed i)    = name i
    name (Tensor t)     = name t
    teXName (Simple s)  = teXName s
    teXName (Indexed i) = teXName i
    teXName (Tensor t)  = teXName t

instance Identified Symbol where
    identifier (Simple s)  = identifier s
    identifier (Indexed i) = identifier i
    identifier (Tensor t)  = identifier t

instance Eq Symbol where
    (==) (Simple x)  (Simple y)  = x == y
    (==) (Simple _)  _           = False
    (==) (Indexed x) (Indexed y) = x == y
    (==) (Indexed _) _           = False
    (==) (Tensor x)  (Tensor y)  = x == y
    (==) (Tensor _)  _           = False

instance Show Symbol where
    showsPrec d (Simple s)  = showsPrec d s
    showsPrec d (Indexed i) = showsPrec d i
    showsPrec d (Tensor t)  = showsPrec d t

instance Commutable Symbol where
    commutativity (Simple s)  = commutativity s
    commutativity (Indexed i) = commutativity i
    commutativity (Tensor t)  = commutativity t


-- If both symbols are non-commuting, compare just
-- returns GT, which will prevent reordering of the
-- factors.

instance Ord Symbol where
    compare (Simple x)  (Simple y)  = compare x y
    compare (Simple _)  _           = GT
    compare (Indexed x) (Indexed y) = compare x y
    compare (Indexed _) _           = GT
    compare (Tensor x)  (Tensor y)  = compare x y
    compare (Tensor _)  _           = GT

instance SumOrd Symbol where
    sumCompare (Simple x) (Simple y)   = compare (name x) (name y)
    sumCompare (Simple _) _            = GT
    sumCompare (Indexed x) (Indexed y) = compare (name x) (name y)
    sumCompare (Indexed _) _           = GT
    sumCompare (Tensor x) (Tensor y)   = compare (name x) (name y)
    sumCompare (Tensor _) _            = GT

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


-- A few handy functions for defining symbols:

simpleSymbol :: String -> Symbol
simpleSymbol s = unsafePerformIO $ do
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = s
                                       , simpleTeXName       = s
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = Commuting
                                       }

ncSymbol :: String -> String -> Symbol
ncSymbol s rep = unsafePerformIO $ do 
                     ident <- nextId
                     return $ Simple S { simpleIdentifier    = ident
                                       , simpleName          = s
                                       , simpleTeXName       = s
                                       , simpleComplexity    = Real
                                       , simpleCommutativity = NonCommuting (RepSpace rep)
                                       }


-- Show the internal details of a Symbol:

showsSymbol_ :: Symbol -> ShowS
showsSymbol_ (Simple s)  = if simpleCommutativity s == Commuting
                               then showString "$ simpleSymbol \"" .
                                    showString (simpleName s) .
                                    showString "\"" 
                               else showString "$ ncSymbol \"" .
                                    showString (simpleName s)  .
                                    showString "\" \""         .
                                    showString (fromJust $ repSpace s)    .
                                    showString "\""
showsSymbol_ (Indexed i) = showString "Indexed " .
                           shows (identifier i)
showsSymbol_ (Tensor t)  = showString "Tensor "  .
                           shows (identifier t)