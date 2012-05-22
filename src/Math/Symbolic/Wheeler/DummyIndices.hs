--
-- DummyIndices.hs
--
-- Take a tensor expression and replace repeated indices
-- with unique dummy indices.
--
-- Gregory Wright, 1 May 2012
--


module Math.Symbolic.Wheeler.DummyIndices (
    uniqueDummies,
    uniqueDummies_
) where


import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Tuple
import System.IO.Unsafe (unsafePerformIO)

import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities



type IndexTrail = (VarIndex, Breadcrumbs)
type IndexList  = [ IndexTrail ]

data Cxt = Scxt Int | Pcxt Int | Tcxt Int
     deriving (Eq, Ord, Show)

type Breadcrumbs = [ Cxt ]

data PairPrefix = PairPrefix { commonPrefix :: Breadcrumbs
                             , prefixDiff   :: Cxt
                             , indexTrail   :: IndexTrail
                             , indexTrail'  :: IndexTrail
                             } deriving (Show)

isProductCxt :: Cxt -> Bool
isProductCxt (Pcxt _) = True
isProductCxt       _ = False

isTensorCxt :: Cxt -> Bool
isTensorCxt (Tcxt _) = True
isTensorCxt       _  = False

isProductOrTensorCxt :: Cxt -> Bool
isProductOrTensorCxt x = isProductCxt x || isTensorCxt x


-- collectIndices records the breadcrumb trail to each index and
-- groups indices by name.  Each IndexList in the result (with more
-- than one entry) is a list of repeated indices that require
-- dummy replacement.
--
-- As of 14 May 12, don't replace indices that are already dummies.
--
collectIndices :: Expr -> [ IndexList ]
collectIndices e = map (sortBy (comparing (length . snd)))     $
                   groupBy (equalling (toContravariant . fst)) $
                   sortBy  (comparing (toContravariant . fst)) $
                   filter (not . isDummy . fst)                $ fi [] [] e
    where
        fi cxt il (Symbol (Tensor t))  = (zipWith (\n i -> (i, Tcxt n : cxt) ) [1..] (slots t)) ++ il
        fi cxt il (Product ps) = concatMap (\(n, x) -> fi ((Pcxt n) : cxt) il x) $ zip [1..] ps
        fi cxt il (Sum  ts)    = concatMap (\(n, x) -> fi ((Scxt n) : cxt) il x) $ zip [1..] ts
        fi _   il _            = il

        equalling p x y = (p x) == (p y)


collectIndices_ :: Expr -> [ IndexList ]
collectIndices_ e = map (sortBy (comparing (length . snd)))     $
                   groupBy (equalling (toContravariant . fst)) $
                   sortBy  (comparing (toContravariant . fst)) $ fi [] [] e
    where
        fi cxt il (Symbol (Tensor t))  = (zipWith (\n i -> (i, Tcxt n : cxt) ) [1..] (slots t)) ++ il
        fi cxt il (Product ps) = concatMap (\(n, x) -> fi ((Pcxt n) : cxt) il x) $ zip [1..] ps
        fi cxt il (Sum  ts)    = concatMap (\(n, x) -> fi ((Scxt n) : cxt) il x) $ zip [1..] ts
        fi _   il _            = il

        equalling p x y = (p x) == (p y)


-- mkReplacements is the driver function for dummy replacement.  It
-- first scans the passed index lists and generates all possible
-- pairings of identically-named indices.
--
mkReplacements :: [ IndexList ] -> [ IndexTrail ]
mkReplacements []  = []
mkReplacements ils = let
        pairs      = filter (not . null) $ map mkPairs ils
        pairables  = map (map findCommonPrefix) pairs
        pairables' = map (sortBy (\x y -> compare (commonPrefix y)
                                                  (commonPrefix x))) pairables

        resolvables = map (partition (isProductOrTensorCxt . prefixDiff)) pairables'

        resolutions (ps, ss) = let
                u = productEdits $ resolveProduct ps
                v = sumEdits ss
            in
                resolveSum u v
    in
        concatMap resolutions resolvables


mkPairs :: IndexList -> [ (IndexTrail, IndexTrail) ]
mkPairs il = concatMap tailPairs $ tails il
    where
        -- tailPairs returns a list of pairs, with the first
        -- element being the head of the list, and the second
        -- element element drawn from the remainder of the list.
        tailPairs []       = []
        tailPairs (_ : []) = []
        tailPairs (x : xs) = [ (x, x') | x' <- xs ]


findCommonPrefix :: (IndexTrail, IndexTrail) -> PairPrefix
findCommonPrefix (x, y) = let
    bc  = reverse (snd x)
    bc' = reverse (snd y)
    p   = span (\(a, b) -> a == b) $ zip bc bc'

    prefix = map fst (fst p)
    dcxt   = fst (head (snd p))
    in
        PairPrefix { commonPrefix = prefix
                   , prefixDiff   = dcxt
                   , indexTrail   = x
                   , indexTrail'  = y
                   }


resolveProduct :: [ PairPrefix ] -> [ PairPrefix ]
resolveProduct [] = []
resolveProduct (p : ps) = pairOff p : resolveProduct (deleteOverlap p ps)
    where
        pairOff :: PairPrefix -> PairPrefix
        pairOff pp = let
                (ind,  bc ) = indexTrail  pp
                (ind', bc') = indexTrail' pp
                mf          = varIndexManifold ind

                uind   = unsafePerformIO $ uniqueDummy mf
                uind'  = if isCovariant ind  then toCovariant uind else uind
                uind'' = if isCovariant ind' then toCovariant uind else uind
            in
                pp { indexTrail = (uind', bc), indexTrail' = (uind'', bc') }

        deleteOverlap x xs = filter (notOverlapping x) xs
            where
                notOverlapping y y' = let
                        t  = snd (indexTrail  y)
                        t' = snd (indexTrail' y)
                        u  = snd (indexTrail  y')
                        u' = snd (indexTrail' y')
                   in
                        not (t == u || t == u' || t' == u || t' == u')


productEdits :: [ PairPrefix ] -> IndexList
productEdits ps = concatMap (\x -> [ indexTrail x, indexTrail' x]) ps


resolveSum :: IndexList -> [ (Breadcrumbs, Breadcrumbs) ] -> IndexList
resolveSum products sums = let
  pm  = Map.fromList (map swap products)
  pm' = foldr matchSum pm sums
  in
   map swap (Map.toList pm')


matchSum :: (Breadcrumbs, Breadcrumbs)
         -> Map.Map Breadcrumbs VarIndex
         -> Map.Map Breadcrumbs VarIndex
matchSum (bc, bc') m = let
  i  = Map.lookup bc  m
  i' = Map.lookup bc' m
  in
   case (i, i') of
     (Just v,  _)       -> Map.insert bc' v  m
     (Nothing, Just v') -> Map.insert bc  v' m
     (Nothing, Nothing) -> m


-- If there is a sum before the first product, then keep
-- the edit.
--
sumEdits :: [ PairPrefix ] ->  [ (Breadcrumbs, Breadcrumbs) ]
sumEdits v = let
  v'  = filter (not . null . commonPrefix) v
  v'' = filter (isProductCxt . last . commonPrefix) v'
  in
   map (\x -> (snd $ indexTrail x, snd $ indexTrail' x)) v''


-- Given an index and a breadcrumb trail, replace the corresponding
-- index in the tree with the supplied index.  The argument order
-- is compatible with using replaceIndex in foldr.
--
replaceIndex :: (VarIndex, Breadcrumbs) -> Expr -> Expr
replaceIndex (i, bc) e = snd $ ri i bc ([], e)
    where
        ri :: VarIndex -> Breadcrumbs -> (Breadcrumbs, Expr) -> (Breadcrumbs, Expr)
        ri i' b' (b, t@(Symbol (Tensor _))) = if (b == tail b')
                                           then (b, repIndex (head b') i' t)
                                           else (b, t)
        ri i' b' (b, Product ps)   = (b, Product (zipWith (\n x -> snd (ri i' b' ((Pcxt n) : b, x))) [1..] ps))
        ri i' b' (b, Sum ts)       = (b, Sum     (zipWith (\n x -> snd (ri i' b' ((Scxt n) : b, x))) [1..] ts))
        ri _  _  u@(_, _)          = u

        repIndex                                  :: Cxt -> VarIndex -> Expr -> Expr
        repIndex (Tcxt n) ind (Symbol (Tensor t)) = Symbol (Tensor $ t {slots = (replace n ind (slots t))})
            where
                replace j x l = map (\(k, y) -> if j == k then x else y) $ zip [1..] l 
        repIndex _ _ _                            = error "Can't happen: error replacing index"


-- Given a tree and an IndexList, replace all of the corresponding
-- indices in the tree.
--
replaceIndices :: Expr -> IndexList -> Expr
replaceIndices t edits = foldr replaceIndex t edits


-- Top level function for replacing repeated indices with unique
-- dummies.
--
uniqueDummies   :: Expr -> Expr
uniqueDummies t = replaceIndices t $ mkReplacements $ collectIndices t


-- Top level function that relabels existing dummy indices
--
uniqueDummies_   :: Expr -> Expr
uniqueDummies_ t = replaceIndices t $ mkReplacements $ collectIndices_ t
