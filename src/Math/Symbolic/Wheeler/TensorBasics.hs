--
-- TensorBasics.hs
--
-- Basic tensor operations: check that a tensor expression
-- is well-formed, identify dummy and free indices.
--
-- Gregory Wright, 31 August 2011
--

module Math.Symbolic.Wheeler.TensorBasics where


import Data.Maybe
import Data.List
import qualified Data.Map as Map
import System.IO.Unsafe


import Math.Symbolic.Wheeler.Basic
import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities


-- Determine if an expression contains a tensor
--
hasTensor :: Expr -> Bool
hasTensor (Const _)     = False
hasTensor (Applic _ _)  = False  -- at the moment, all applications yield scalars
hasTensor (Symbol s)    = isTensor s
hasTensor (Sum ts)      = any hasTensor ts
hasTensor (Product fs)  = any hasTensor fs
hasTensor (Power b _)   = hasTensor b
hasTensor Undefined     = False


-- The kernel letter of a tensor:
--
tensorKernel :: Expr -> Maybe String
tensorKernel (Symbol (Tensor t)) = Just (tensorName t)
tensorKernel _                   = Nothing


-- Check if every term in the expansion of an expression
-- has a tensor.  If it does not, the expression is malformed.
--
allTermsTensor :: Expr -> Bool
allTermsTensor e =
    let
        e' = terms (expand e)
    in
        (all hasTensor e') || (all (not . hasTensor) e')


-- Given an expression, get the indices of every term.
--
getAllIndices :: Expr -> [[ VarIndex ]]
getAllIndices e =
    let
        e' = terms (expand e)
    in concatMap getIndices e'


-- Get the indices of an expression.
--
getIndices :: Expr -> [[ VarIndex ]]
getIndices (Const _)     = []
getIndices (Applic _ _)  = []
getIndices (Symbol s)    = [ getSymbolIndices s ]
getIndices (Sum ts)      = concatMap getIndices ts
getIndices (Product fs)  = [ concat $ concatMap getIndices fs ]
getIndices (Power _ _)   = []
getIndices Undefined     = []


getSymbolIndices :: Symbol -> [ VarIndex ]
getSymbolIndices (Simple _)      = []
getSymbolIndices (Indexed _)     = []
getSymbolIndices (Tensor t)      = slots t
getSymbolIndices (DiracSpinor _) = []


-- Partition a list of indices into the free indices, and the
-- dummy indices:
--
partitionIndices :: [ VarIndex ] -> ([ VarIndex ], [ VarIndex ])
partitionIndices is = pindices [] [] is
    where
        pindices free dummy []       = (free, dummy)
        pindices free dummy (s : []) = (s : free, dummy)
        pindices free dummy (s : ss) =
            if not $ elem (-s) ss
                then pindices (s : free) dummy ss
                else pindices free ((toContravariant s) : dummy) (delete (-s) ss)


exprIndices :: Expr -> [ ([ VarIndex ], [ VarIndex ]) ]
exprIndices = (map partitionIndices) . getIndices 


-- Check that the indices of each term of a tensor expression are
-- the same.  An expression with no tensor subexpressions is considered
-- a well-formed tensor expression.
--
-- There are three separate checks here.  The first is to
-- check if there are any repeated indices in the free list.  This
-- is an error.  Then we check if any indices on the free list are
-- also in the dummy list.  This can happen, for example, in the
-- incorrect expression V mu * V(-mu) * V mu.  Finally, we check that
-- every term of the expanded expression has the same index structure.
--
data TensorError = RepeatedFreeIndices
                 | BothFreeAndDummy
                 | TermsNotAllSame

instance Show TensorError where
    showsPrec _ RepeatedFreeIndices = showString "Tensor error: repeated free index"
    showsPrec _ BothFreeAndDummy    = showString "Tensor error: an index is used both free and dummy"
    showsPrec _ TermsNotAllSame     = showString "Tensor error: not all terms have the same index structure"

checkTensor :: Expr -> Either TensorError Expr
checkTensor e =
    if hasTensor e
        then
            let
                i  = exprIndices (expand e)
            in
                if repeatedFrees i then Left RepeatedFreeIndices
                    else if bothFreeAndDummy i then Left BothFreeAndDummy
                        else if not (allSame i) then Left TermsNotAllSame
                            else Right e
        else Right e


-- Make sure the tensor structure of each
-- term is the same as the first:
--
allSame :: Eq a => [ a ] -> Bool
allSame i = all ((==) (head i)) (tail i)


-- Are any free indices repeated?
--
repeatedFrees :: [ ( [ VarIndex ], a) ] -> Bool
repeatedFrees = any (hasRepeats . fst)

-- Are any free indices also on the dummy list?
--
bothFreeAndDummy :: [ ( [ VarIndex ], [ VarIndex ]) ] -> Bool
bothFreeAndDummy = any (\x -> hasCommonElement (fst x) (snd x))

-- hasRepeats tests if any of the elements of a list are repeated.
-- If so, it returns True.
--
hasRepeats :: Eq a => [ a ] -> Bool
hasRepeats [] = False
hasRepeats (x : xs) = elem x xs || hasRepeats xs


-- Another (better? faster?) way to implement the above:
-- sort the list first, then check if any adjacent elements
-- are equal.
--
hasRepeats' :: Ord a => [ a ] -> Bool
hasRepeats' xs = equalAdjacent (sort xs)
    where
        equalAdjacent []       = False
        equalAdjacent (_ : []) = False
        equalAdjacent (x : ys@(y : _)) = x == y || equalAdjacent ys


-- Returns True if the lists x and y have at least one
-- index, either covariant or contravariant, in common.
--
hasCommonElement :: [ VarIndex ] -> [ VarIndex ] -> Bool
hasCommonElement x y = not (null (intersectBy indexNameEq x y))
    where
        indexNameEq (Covariant i)     (Covariant j)     = i == j
        indexNameEq (Covariant i)     (Contravariant j) = i == j
        indexNameEq (Contravariant i) (Covariant j)     = i == j
        indexNameEq (Contravariant i) (Contravariant j) = i == j


-- makeDummiesUnique takes all dummy indices and replaces them
-- with an internally generated index name guaranteed to be unique.
--
data Cxt = EmptyCxt | SumCxt [ Int ] | ProdCxt [ Int ]
     deriving (Eq, Ord, Show)

fidx :: Expr -> Map.Map (VarIndex, Cxt) VarIndex
fidx e = fi Map.empty EmptyCxt e
    where
        fi m cxt (Symbol (Tensor t)) = let
                                           s = slots t
                                           pv i m' = let
                                                      i'  = if isCovariant i then (-i) else i
                                                      i'' = Map.lookup (i', cxt) m'
                                                      mf  = varIndexManifold i
                                                  in
                                                      if isDummy i
                                                          then m'
                                                          else if isJust i''
                                                              then Map.insert (i', cxt) (unsafePerformIO $ uniqueDummy mf) m'
                                                              else Map.insert (i', cxt) i' m'
                                       in
                                           foldr pv m s
        fi m _ (Symbol _)            = m
        fi m EmptyCxt (Product fs)   = foldr (\x m' -> fi m' (ProdCxt []) x) m fs
        fi m (SumCxt c) (Product fs) = foldr (\x m' -> fi m' (ProdCxt  c) x) m fs
        fi m cxt (Product fs)        = foldr (\x m' -> fi m' cxt x) m fs
        fi m EmptyCxt (Sum ts)       = foldr (\(n, x) m' -> fi m' (SumCxt [n]) x) m (zip [1..] ts)
        fi m (SumCxt c) (Sum ts)     = foldr (\(n, x) m' -> fi m' (SumCxt (n : c)) x) m (zip [1..] ts)
        fi m cxt (Sum fs)            = foldr (\x m' -> fi m' cxt x) m fs
        fi m _ (Power _ _)           = m
        fi m _ (Applic _ _)          = m
        fi m _ (Const _)             = m
        fi m _ Undefined             = m

ridx :: Expr -> Map.Map (VarIndex, Cxt) VarIndex -> Expr
ridx e indexMap = ri indexMap EmptyCxt e
    where
        ri m cxt (Symbol (Tensor t)) = let
                                           s = slots t
                                           lk i = let
                                                      i'  = if isCovariant i then (-i) else i
                                                      i'' = Map.lookup (i', cxt) m
                                                  in
                                                      if isJust i''
                                                          then if isCovariant i then (-(fromJust i'')) else fromJust i''
                                                          else i
                                           i''' = map lk s
                                       in
                                           Symbol (Tensor (t { slots = i'''} ))
        ri _ _ s@(Symbol _)          = s
        ri m EmptyCxt (Product fs)   = Product (map (\x -> ri m (ProdCxt []) x) fs)
        ri m (SumCxt c) (Product fs) = Product (map (\x -> ri m (ProdCxt  c) x) fs)
        ri m cxt (Product fs)        = Product (map (\x -> ri m cxt x) fs)
        ri m EmptyCxt (Sum ts)       = Sum (zipWith (\n x -> ri m (SumCxt [n]) x) [1..] ts)
        ri m (SumCxt c) (Sum ts)     = Sum (zipWith (\n x -> ri m (SumCxt (n : c)) x) [1..] ts)
        ri m cxt (Sum ts)            = Sum (map (\x -> ri m cxt x) ts)
        ri _ _ p@(Power _ _)         = p
        ri _ _ a@(Applic _ _)        = a
        ri _ _ c@(Const _)           = c
        ri _ _ Undefined             = Undefined

makeDummiesUnique :: Expr -> Expr
makeDummiesUnique x = ridx x (fidx x)


