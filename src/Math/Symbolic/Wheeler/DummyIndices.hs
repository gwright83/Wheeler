--
-- DummyIndices.hs
--
-- Take a tensor expression and replace repeated indices
-- with unique dummy indices.
--
-- Gregory Wright, 1 May 2012
--


module Math.Symbolic.Wheeler.DummyIndices (
  IndexList,
  VarIndexInContext (..),
  collectIndices,
  collectIndices_,
  collectIndices__,
  replaceIndex,
  uniqueDummies,
  uniqueDummies_,
  uniqueDummies__
) where

  
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import System.IO.Unsafe (unsafePerformIO)

import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Symbol
import {-# SOURCE #-} Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities


data VarIndexInContext = VarIndexInContext {
  index   :: VarIndex,
  context :: [ Cxt ]
} deriving (Show)

instance Eq VarIndexInContext where
  (==) i j = toContravariant (index i) == toContravariant (index j)

instance Ord VarIndexInContext where
  compare i j = compare (toContravariant $ index i) (toContravariant $ index j)
  
  
type IndexList      =  [ VarIndexInContext ]
type MatchedIndices = [[ VarIndexInContext ]]


-- collectIndices records the breadcrumb trail to each index and
-- groups indices by name.  Each IndexList in the result (with more
-- than one entry) is a list of repeated indices that require
-- dummy replacement.
--
-- As of 14 May 12, don't replace indices that are already dummies.
--
resolveDummies :: (VarIndex -> Bool) -> Expr -> MatchedIndices
resolveDummies itest ex = fst $ findIndexPairs ex []
  where
    cxtList cxtName cxt = map (\i -> (cxtName i) : cxt) [1.. ]
    
    findIndexPairs :: Expr -> [ Cxt ] -> ( MatchedIndices, IndexList )
    findIndexPairs expr cxt = case expr of
      (Sum ts)      -> let  
        contexts  = cxtList Scxt cxt
        indexList = zipWith findIndexPairs ts contexts
        in pairableSumIndices indexList
        
      (Product fs)  -> let
        contexts  = cxtList Pcxt cxt
        indexList = zipWith findIndexPairs fs contexts
        in pairProductIndices indexList
        
      (Symbol (Tensor t)) -> let
        contexts = cxtList Tcxt cxt
        ics      = filter (itest . index) $
                   zipWith (\i c -> VarIndexInContext { index   = i
                                                      , context = c }) (slots t) contexts
        in groupEquals ics
           
      _ -> ( [], [] )



excludeNone :: VarIndex -> Bool
excludeNone = const True

excludeDummies :: VarIndex -> Bool
excludeDummies = not . isDummy

excludeDummiesAndPatterns :: VarIndex -> Bool
excludeDummiesAndPatterns i = not (isPatternVarIndex i || isDummy i) 


pairableSumIndices :: [ ( MatchedIndices, IndexList ) ]    
                   ->   ( MatchedIndices, IndexList )
pairableSumIndices termIndices = let
  previouslyPaired    = concatMap fst termIndices
  potentiallyPairable = concat $ elementsOfEach $ map snd termIndices
  in
   (previouslyPaired, potentiallyPairable)
  
  
pairProductIndices :: [ ( MatchedIndices, IndexList ) ]    
                   ->   ( MatchedIndices, IndexList )
pairProductIndices factorIndices = let
  previouslyPaired         = concatMap fst factorIndices
  (newlyPaired, notPaired) = presentInTwoLists $ map snd factorIndices 
  in (previouslyPaired ++ newlyPaired, notPaired)


presentInTwoLists :: [ IndexList ] -> ( MatchedIndices, IndexList )
presentInTwoLists il = let
  
  indexAssoc :: [ (VarIndexInContext, [ (VarIndexInContext, Int) ]) ]
  indexAssoc = concat $
               zipWith (\x y ->
                         zipWith (\x' y' -> (x', [ (x', y') ])) x (repeat y)
                       ) il [1..]
            
  indexMap        = Map.fromListWith (++) indexAssoc
  
  fromDifferentLists [] = False
  fromDifferentLists ((_, x) : xs) = not $ all (\y -> x == snd y) xs
  
  (repeatedIndices', unmatchedIndices) = Map.partition fromDifferentLists indexMap

  repeatedIndices'' = map (map fst) $ (map snd) (Map.toList repeatedIndices')
  unmatchedIndices' = concatMap (map fst) $ (map snd) (Map.toList unmatchedIndices)
  in
    (repeatedIndices'', unmatchedIndices')



-- elementsOfEach takes a list of lists and returns a list
-- of elements appearing in every one of the input lists.
--
elementsOfEach :: [[ VarIndexInContext ]]
               -> [[ VarIndexInContext ]]
elementsOfEach []          = []
elementsOfEach l@(xs : _)  = let
  xs' = nub xs
  listToMaybe' y = if null y then Nothing else Just y
  in
   map concat $
   catMaybes  $
   map (\x -> sequence $ map
              (listToMaybe' . filter ((==) x)) l
       ) xs'


-- groupEq takes a list and returns a pair: the first
-- element of the pair is a list of lists, each list containing
-- the elements of the input list that are repeated.  The
-- second element of the pair is the list of singletons.
--
groupEquals ::   [ VarIndexInContext ]
            -> ([[ VarIndexInContext ]], [ VarIndexInContext ])
groupEquals []       = ([], [])
groupEquals (x : []) = ([], [ x ])
groupEquals (x : xs) = geq [] [] x xs
  where
    geq :: [[ VarIndexInContext ]]                             -- accumulator list of matches
        ->   [ VarIndexInContext ]                             -- accumulator list of non-matches
        ->     VarIndexInContext                               -- item to test for match
        ->   [ VarIndexInContext ]                             -- input list
        -> ([[ VarIndexInContext ]], [ VarIndexInContext ])    -- pair of ([matches], nonmatches)
    geq macc nacc y [] = (macc, y : nacc)
    geq macc nacc y ys = let
      (ms, nms) = partition ((==) y) ys
      in if null nms
         then ((y : ms) : macc, nacc)
         else if null ms
              then geq macc (y : nacc) (head nms) (tail nms)
              else geq ((y : ms) : macc) nacc (head nms) (tail nms)
  

-- mkReplacements is the driver function for dummy replacement.  It
-- first scans the passed index lists and generates all possible
-- pairings of identically-named indices.
--
mkReplacements :: MatchedIndices -> IndexList
mkReplacements []  = []
mkReplacements mss = concatMap mkDummies mss
  where
    mkDummies :: IndexList -> IndexList
    mkDummies ms = let
      mf   = varIndexManifold (index (head ms))
      repl = unsafePerformIO $ uniqueDummy mf
      
      replaceWithDummy rind ind = if isCovariant (index ind)
                                  then ind { index = toCovariant $ rind }
                                  else ind { index = rind }
      in 
       map (replaceWithDummy repl) ms


-- Given an index and a breadcrumb trail, replace the corresponding
-- index in the tree with the supplied index.  The argument order
-- is compatible with using replaceIndex in foldr.
--
replaceIndex :: VarIndexInContext -> Expr -> Expr
replaceIndex v e = snd $ ri (index v) (context v) ([], e)
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


-- Given a tree and an MatchedIndices list, replace all of the corresponding
-- indices in the tree.
--
replaceIndices :: Expr -> IndexList -> Expr
replaceIndices t edits = foldr replaceIndex t edits


collectIndices   :: Expr -> MatchedIndices
collectIndices   = resolveDummies excludeDummies

collectIndices_  :: Expr -> MatchedIndices
collectIndices_  = resolveDummies excludeNone

collectIndices__ :: Expr -> MatchedIndices
collectIndices__ = resolveDummies excludeDummiesAndPatterns


-- Top level function for replacing repeated indices with unique
-- dummies.
--
uniqueDummies   :: Expr -> Expr
uniqueDummies t = replaceIndices t $
                  mkReplacements   $
                  collectIndices t


-- Top level function that relabels existing dummy indices
--
uniqueDummies_   :: Expr -> Expr
uniqueDummies_ t = replaceIndices t $
                   mkReplacements   $
                   collectIndices_ t


-- Top level function that relabels repeated indices, skipping pattern indices
--
uniqueDummies__   :: Expr -> Expr
uniqueDummies__ t = replaceIndices t $
                    mkReplacements   $
                    collectIndices__ t
