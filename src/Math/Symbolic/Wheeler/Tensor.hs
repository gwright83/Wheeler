{-# LANGUAGE FlexibleContexts #-}
--
-- Tensor.hs
--
-- Try to cleanly handle indexed objects.
--
-- Gregory Wright, 18 March 2011
--                 24 August 2011
--


module Math.Symbolic.Wheeler.Tensor where

import Data.Maybe
import Prelude hiding ((^))
import System.IO.Unsafe

import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Complexity
import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Named
import {-# SOURCE #-} Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.TensorComponents
import Math.Symbolic.Wheeler.TensorSymmetries
import Math.Symbolic.Wheeler.UniqueID


-- Our basic object is the tensor.  The tensor is a record made
-- from the parent manifold, a field describing the type of the
-- tensor (e.g., metric, Kronecker delta, etc.), a list of the slots,
-- the slot symmetries, given as a base and a generating set,
-- and possibly an array of component expressions.
--
-- The slots are occupied by abstract indices or by component
-- indices.
--
-- For use in programs, a more convenient representation of a
-- tensor is the "TensorExpr".  It is a function from a list of
-- slots to an expression.  A function of type TensorExpr takes
-- a list of slots and returns an complete Symbol with tensorality
-- field equal to the completed Tensor record.
--
--
data T = T {
     tensorIdentifier    :: Id,
     tensorName          :: String,
     tensorTeXName       :: String,
     manifold            :: Manifold,    -- the parent manifold of the tensor field
     tensorType          :: TensorType,
     slots               :: [ VarIndex ],
     symmetry            :: Symmetry,
     tensorComplexity    :: Complexity,
     tensorCommutativity :: Commutativity,
     components          :: Maybe TensorComponents
}

-- Think about the Eq instance for tensor.  Should we
-- use a unique ID here?  How should tensors with the same
-- name but different indices be distinguished?
--
-- 11 Feb 2012 updated to compare the tensorName (should
-- that field be renamed "kernel symbol"?) and the slots.
--
instance Eq T where
    (==) x y = (tensorName x == tensorName y) &&
               (slots x == slots y)

instance Ord T where
    compare _ _ = GT

instance Named T where
    name    = tensorName
    teXName = tensorTeXName

instance Identified T where
    identifier = tensorIdentifier

instance Show T where
    showsPrec d t = showString (tensorName t) .
                    showString " "            .
                    showsList_ (showsPrec d) (slots t)

showsList_ :: (a -> ShowS) -> [a] -> ShowS
showsList_ _     []     s = s
showsList_ showx (x:xs) s = showx x (showl xs)
    where
        showl []     =  s
        showl (y:ys) = ' ' : showx y (showl ys)



instance Commutable T where
    commutativity = tensorCommutativity


type TensorExpr_2      = VarIndex -> VarIndex -> Expr
type NamedTensorExpr_2 = String -> String -> VarIndex -> VarIndex -> Expr


-- A short list of tensor types.  The most important thing is to
-- distinguish the special tensors Metric, LeviCivita and KroneckerDelta
-- from a general tensor.
--
-- The signature of the metric, for manifolds possessing a metric,
-- is kept in the manifold record, not the tensor record.
--
data TensorType = General
                | Metric
                | KroneckerDelta
                | LeviCivita


-- A slot is either occupied by covariant index (indicating
-- that the slot takes a vector from the cotangent space)
-- or contravariant index (indicating that the slot takes
-- a vector from the tangent space).
--
-- VarIndices are instances of Ord so they can be sorted for
-- easy comparison.  For example, this makes it simpler to
-- check the all terms of a tensor expression have the same
-- index structure, a requirement if the expression is well-
-- formed.
--
data VarIndex = Covariant Idx
              | Contravariant Idx
    deriving Eq

instance Ord VarIndex where
    compare (Covariant     m) (Covariant     n) = compare m n
    compare (Covariant     _) (Contravariant _) = LT
    compare (Contravariant _) (Covariant     _) = GT
    compare (Contravariant m) (Contravariant n) = compare m n

instance Show VarIndex where
    showsPrec _ (Contravariant (Abstract i))  = showString (indexName i)
    showsPrec _ (Covariant     (Abstract i))  = showString "(-"          .
                                                showString (indexName i) .
                                                showString ")"
    showsPrec _ (Contravariant (Component i)) = showString (show i)
    showsPrec _ (Covariant     (Component i)) = showString "(-"     .
                                                showString (show i) .
                                                showString ")"


-- An index itself can either be abstract (see
-- <http://en.wikipedia.org/wiki/Abstract_index_notation>) or
-- a component index.  For the moment, component indices
-- do nothing, but later they will allow specifying the
-- symbolic expression corresponding to a tensor component.
--
data Idx = Abstract Index
         | Component Int
    deriving (Eq, Show)

instance Ord Idx where
    compare (Abstract  m) (Abstract  n) = compare n m
    compare (Abstract  _) (Component _) = LT
    compare (Component _) (Abstract  _) = GT
    compare (Component m) (Component n) = compare m n


-- The IndexName is the textual representation of the index.
-- The indexName field is the unique identifier of the index
-- (since it is used to define the Eq instance of the type).
--
data Index = Index {
   indexManifold :: Manifold,
   indexName     :: String,
   indexTeXName  :: String
} deriving Show

instance Eq Index where
    (==) m n = ((indexManifold m) == (indexManifold n)) &&
               ((indexName m)     == (indexName n))

instance Ord Index where
    compare m n = compare (indexName m) (indexName n)

instance Named Index where
    name    = indexName
    teXName = indexTeXName


-- A simple operator to toggle the variance.
-- It is an ugly hack, but letting "-" toggle the
-- variance is the least ugly option, given that we
-- don't have unary operators in Haskell.
--
-- The unused instances use "error" because there
-- is no sensible recovery from these illegal operations.
--
-- Should "negate" be in a new class by itself?
--
instance Num VarIndex where
    negate (Covariant i)     = Contravariant i
    negate (Contravariant i) = Covariant i
    (+) _ _       = error "can't add slots"
    (*) _ _       = error "can't multiply slots"
    abs _         = error "can't take abs of a slot"
    signum _      = error "can't take signum of a slot"
    fromInteger _ = error "can't convert Integer to slot"


-- The Signature is the signature of a symmetric matrix.
-- The signaure of an antisymmetric metric can be determined
-- just from the dimension, since the eigenvalues of an
-- antisymmetric matrix occur in pairs +/- i * lambda,
-- with lambda real.
--
type Signature = (Int, Int)
data Metricity = NoMetric
               | Symmetric Signature
               | Antisymmetric
    deriving (Eq, Show)

-- Metricity abbreviations for common spaces:
--
minkowski :: Metricity
minkowski   = Symmetric (1,3)

euclidean :: Int -> Metricity
euclidean n = Symmetric (n, 0)


-- The Manifold record gives the name, dimension and indices
-- associated with the tangent bundle of the manifold.
--
data Manifold = Manifold {
    manifoldName           :: String,
    manifoldTeXName        :: String,
    manifoldDimension      :: Int,
    manifoldDimensionDelta :: Maybe Expr,
    metricity              :: Metricity
} deriving (Eq, Show)

instance Named Manifold where
    name    = manifoldName
    teXName = manifoldTeXName


-- mkManifold make a new manifold and possibly a metric
-- tensor.
--
mkManifold :: String         -- name of the manifold
           -> Int            -- dimension of the manifold
           -> Metricity      -- the metricity, as defined above
           -> (Manifold, Maybe NamedTensorExpr_2)
mkManifold nam dim metr =
    let
        manif = Manifold { manifoldName           = nam,
                           manifoldTeXName        = nam,
                           manifoldDimension      = dim,
                           manifoldDimensionDelta = Nothing,
                           metricity              = metr }

        maybeMetric :: Maybe NamedTensorExpr_2
        maybeMetric = if metricity manif == NoMetric
                          then Nothing
                          else Just (mkNamedMetric manif)
    in
        (manif, maybeMetric)


checkIndicesInManifold :: Manifold -> [ VarIndex ] -> Bool
checkIndicesInManifold m indices =
    let
        getIndex :: VarIndex -> Index
        getIndex (Covariant     (Abstract s))  = s
        getIndex (Covariant     (Component _)) = error "given component instead of abstract index"
        getIndex (Contravariant (Abstract s))  = s
        getIndex (Contravariant (Component _)) = error "given component instead of abstract index"

        getIndexManifold :: VarIndex -> Manifold
        getIndexManifold s = indexManifold (getIndex s)

        manifolds = map getIndexManifold indices
    in
        all ((==) m) manifolds


mkNamedMetric :: Manifold
              -> String
              -> String
              -> VarIndex
              -> VarIndex
              -> Expr
mkNamedMetric m nam teXNam i1 i2 =
    let
        indices = [ i1, i2 ]
    in
        if  checkIndicesInManifold m indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = nam
                                               , tensorTeXName       = teXNam
                                               , manifold            = m
                                               , tensorType          = Metric
                                               , slots               = indices
                                               , symmetry            = symmetricTwoIndex
                                               , tensorComplexity    = Real
                                               , tensorCommutativity = Commuting
                                               , components          = Nothing }
            else error ("index error defining metric " ++ nam)


mkVector :: Manifold
         -> String
         -> VarIndex
         -> Expr
mkVector m nam i =
    let
        indices = [ i ]
    in
        if  checkIndicesInManifold m indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = nam
                                               , tensorTeXName       = nam
                                               , manifold            = m
                                               , tensorType          = General
                                               , slots               = indices
                                               , symmetry            = oneIndex
                                               , tensorComplexity    = Real
                                               , tensorCommutativity = Commuting
                                               , components          = Nothing }
            else error ("index error defining metric " ++ nam)


mkVector_ :: Manifold
          -> String
          -> String
          -> VarIndex
          -> Expr
mkVector_ m nam teXNam i =
    let
        indices = [ i ]
    in
        if  checkIndicesInManifold m indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = nam
                                               , tensorTeXName       = teXNam
                                               , manifold            = m
                                               , tensorType          = General
                                               , slots               = indices
                                               , symmetry            = oneIndex
                                               , tensorComplexity    = Real
                                               , tensorCommutativity = Commuting
                                               , components          = Nothing }
            else error ("index error defining metric " ++ nam)

                         
mkKroneckerDelta :: Manifold
                 -> String
                 -> VarIndex
                 -> VarIndex
                 -> Expr
mkKroneckerDelta m nam i1@(Covariant _) i2@(Contravariant _) =
    let
        indices = [ i1, i2 ]
    in
        if  checkIndicesInManifold m indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = nam
                                               , tensorTeXName       = nam
                                               , manifold            = m
                                               , tensorType          = KroneckerDelta
                                               , slots               = indices
                                               , symmetry            = symmetricTwoIndex
                                               , tensorComplexity    = Real
                                               , tensorCommutativity = Commuting
                                               , components          = Nothing }
            else error ("Kronecker delta indices are not all in manifold " ++ name m)
mkKroneckerDelta _ _ _ _ = error "Kronecker delta must have ordered Covariant, Contravariant indices "
                          

mkKroneckerDelta_ :: Manifold
                  -> String
                  -> String
                  -> VarIndex
                  -> VarIndex
                  -> Expr
mkKroneckerDelta_ m nam teXNam i1@(Covariant _) i2@(Contravariant _) =
    let
        indices = [ i1, i2 ]
    in
        if  checkIndicesInManifold m indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = nam
                                               , tensorTeXName       = teXNam
                                               , manifold            = m
                                               , tensorType          = KroneckerDelta
                                               , slots               = indices
                                               , symmetry            = symmetricTwoIndex
                                               , tensorComplexity    = Real
                                               , tensorCommutativity = Commuting
                                               , components          = Nothing }
            else error ("Kronecker delta indices are not all in manifold " ++ name m)
mkKroneckerDelta_ _ _ _ _ _ = error "Kronecker delta must have ordered Covariant, Contravariant indices "

                         
mkLeviCivita :: Manifold
             -> String
             -> VarIndex
             -> VarIndex
             -> VarIndex
             -> VarIndex
             -> Expr
mkLeviCivita m nam i1@(Contravariant _) i2@(Contravariant _) i3@(Contravariant _) i4@(Contravariant _) =
    let
        indices = [ i1, i2, i3, i4 ]
    in
        if  checkIndicesInManifold m indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = nam
                                               , tensorTeXName       = nam
                                               , manifold            = m
                                               , tensorType          = LeviCivita
                                               , slots               = indices
                                               , symmetry            = antisymmetricFourIndex
                                               , tensorComplexity    = Real
                                               , tensorCommutativity = Commuting
                                               , components          = Nothing }
            else error ("Levi-Civita indices are not all in manifold " ++ name m)
mkLeviCivita _ _ _ _ _ _ = error "Levi-Civita must have all Covariant indices "
                          

                         
mkLeviCivita_ :: Manifold
              -> String
              -> String
              -> VarIndex
              -> VarIndex
              -> VarIndex
              -> VarIndex
              -> Expr
mkLeviCivita_ m nam teXNam i1@(Contravariant _) i2@(Contravariant _) i3@(Contravariant _) i4@(Contravariant _) =
    let
        indices = [ i1, i2, i3, i4 ]
    in
        if  checkIndicesInManifold m indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = nam
                                               , tensorTeXName       = teXNam
                                               , manifold            = m
                                               , tensorType          = LeviCivita
                                               , slots               = indices
                                               , symmetry            = antisymmetricFourIndex
                                               , tensorComplexity    = Real
                                               , tensorCommutativity = Commuting
                                               , components          = Nothing }
            else error ("Levi-Civita indices are not all in manifold " ++ name m)
mkLeviCivita_ _ _ _ _ _ _ _ = error "Levi-Civita must have all Covariant indices "
                          

