--
-- TensorBasics.hs
--
-- Basic tensor operations: check that a tensor expression
-- is well-formed, identify dummy and free indices.
--
-- Gregory Wright, 31 August 2011
--

module Math.Symbolic.Wheeler.TensorBasics where


--import Data.Either
import Data.List


import Math.Symbolic.Wheeler.Basic
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor


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
getSymbolIndices (Simple _)  = []
getSymbolIndices (Indexed _) = []
getSymbolIndices (Tensor t)  = slots t


-- Partition a list of indices into the free indices, and the
-- dummy indices:
--
partitionIndices :: [ VarIndex ] -> ([ VarIndex ], [ VarIndex ])
partitionIndices is = pindices [] [] is
    where
        toContravariant s@(Contravariant _) = s
        toContravariant   (Covariant s)     = Contravariant s
 
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
