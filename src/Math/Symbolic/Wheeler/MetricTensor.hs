{-# LANGUAGE ScopedTypeVariables #-}
--
-- MetricTensor.hs
--
-- Eliminate metric and kronecker tensors from expressions.
--
-- Gregory Wright, 25 May 2012
--

module Math.Symbolic.Wheeler.MetricTensor where


import Data.List
import Data.Maybe

import Math.Symbolic.Wheeler.Basic
import Math.Symbolic.Wheeler.Canonicalize
import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.DummyIndices
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Numeric
import Math.Symbolic.Wheeler.Replacer
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities



hasDummies :: Expr -> Bool
hasDummies (Symbol (Tensor t)) = any isDummy $ slots t
hasDummies _ = False


gatherLeaves :: forall a . (Expr -> Breadcrumbs -> Maybe a) -> Expr -> [ a ]
gatherLeaves leafFn expr = gl [] expr
  where
    cxtList cxtName cxt = map (\i -> (cxtName i) : cxt) [1.. ]
    
    gl :: Breadcrumbs -> Expr -> [ a ]
    gl bc e = case e of
      (Sum ts)     -> let
        bcs = cxtList Scxt bc
        in concat $ zipWith gl bcs ts
           
      (Product fs) -> let
        bcs = cxtList Pcxt bc
        in concat $ zipWith gl bcs fs
           
      s@(Symbol _) -> let
        l = leafFn s bc
        in if isJust l then [ fromJust l ] else []
                                            
      c@(Const _)  -> let
        l = leafFn c bc
        in if isJust l then [ fromJust l ] else []
                                                
      _ -> []
    
metricDummies :: Expr -> Breadcrumbs -> Maybe [ VarIndexInContext ]
metricDummies t@(Symbol (Tensor t')) bc =
  if isMetric t && hasDummies t
  then Just $ zipWith (\n i -> VarIndexInContext { index   = i
                                                 , context = Tcxt n : bc }) [1..] (slots t')
  else Nothing
metricDummies _ _ = Nothing


metricDummyIndices :: Expr -> [[ VarIndexInContext ]]
metricDummyIndices ex = gatherLeaves metricDummies ex


metricDummyIndices_ :: [[ VarIndexInContext ]] -> [ VarIndexInContext ]
metricDummyIndices_ il = map toEdit il
    where
      toEdit (i : i' : []) = if not (isDummy (index i))
                             then VarIndexInContext { index   = index i
                                                    , context = context i' }
                             else VarIndexInContext { index   = index i' 
                                                    , context = context i }
      toEdit _             = error "metricDummyIndices_ applied to ill-formed list"
      

metricDummyEdits :: Expr -> [ VarIndexInContext ]
metricDummyEdits = metricDummyIndices_ . metricDummyIndices

findDummyMetric :: Expr -> Maybe Expr
findDummyMetric = findExpr isDummyMetric


isDummyMetric :: Expr -> Bool
isDummyMetric t@(Symbol (Tensor t')) = isMetric t   &&
                                       hasDummies t &&
                                       not (contractedPair (slots t'))
isDummyMetric _ = False


isDummyKronecker :: Expr -> Bool
isDummyKronecker t@(Symbol (Tensor t')) = isKroneckerDelta t &&
                                          hasDummies t       &&
                                          not (contractedPair (slots t'))

isDummyKronecker _ = False


isContractedKronecker :: Expr -> Bool
isContractedKronecker t@(Symbol (Tensor t')) = (isMetric t || isKroneckerDelta t) &&
                                               hasDummies t                       &&
                                               (contractedPair (slots t'))
isContractedKronecker _ = False


contractedPair :: [ VarIndex ] -> Bool
contractedPair (x : y : []) = x == (-y)
contractedPair _ = False


findExprs :: (Expr -> Bool) -> Expr -> [ (Expr, Breadcrumbs) ]
findExprs p e = fe p [] [] e
    where
      fe p' cxt bl e' = if p' e' then (e', cxt) : bl else fe' p' cxt bl e'
      
      fe' p' cxt bl (Product ps) = concatMap (\(n, x) -> fe p' ((Pcxt n) : cxt) bl x) $ zip [1..] ps
      fe' p' cxt bl (Sum ts)     = concatMap (\(n ,x) -> fe p' ((Scxt n) : cxt) bl x) $ zip [1..] ts
      fe' _ _   bl _             = bl
      
      
findExpr :: (Expr -> Bool) -> Expr -> Maybe Expr
findExpr p e = let
  matches = listToMaybe $ findExprs p e
  in
   if isJust matches then Just (fst $ fromJust matches) else Nothing
                                                             
                                                             
findExpr' :: (Expr -> Bool) -> Expr -> Maybe (Expr, Breadcrumbs)
findExpr' p e = listToMaybe $ findExprs p e


eliminateOneMetric :: Expr -> Expr
eliminateOneMetric e =  let
  metricIndices        = findExpr' isDummyMetric e
  (mi : mi' : [])      = tensorIndices $ fst $ fromJust metricIndices
  metricLoc            = snd $ fromJust metricIndices
  exprIndices          = concat $ collectIndices_ e
  (oldIndex, newIndex) = if isDummy mi then ((-mi), mi') else ((-mi'), mi)
  indexLoc             = find (\x -> index x == oldIndex) exprIndices
  in
   if isJust metricIndices && isJust indexLoc
      then replaceIndex (VarIndexInContext { index   = newIndex
                                           , context = context $ fromJust indexLoc })
                        (replaceExpr metricLoc (Const 1) e) 
      else e


eliminateMetric :: Expr -> Expr
eliminateMetric e = let
  e'  = uniqueDummies_ $ expand e
  ts  = terms e'
  e'' = map (applyUntilStable eliminateOneMetric) ts
  in
   canonicalize (Sum e'')
   

eliminateOneKronecker :: Expr -> Expr
eliminateOneKronecker e =  let
  kroneckerIndices     = findExpr' isDummyKronecker e
  (ki : ki' : [])      = tensorIndices $ fst $ fromJust kroneckerIndices
  kroneckerLoc         = snd $ fromJust kroneckerIndices
  exprIndices          = concat $ collectIndices_ e
  (oldIndex, newIndex) = if isDummy ki then ((-ki), ki') else ((-ki'), ki)
  indexLoc             = find (\x -> index x == oldIndex) exprIndices
  in
   if isJust kroneckerIndices && isJust indexLoc
      then replaceIndex (VarIndexInContext { index   = newIndex
                                           , context = context $ fromJust indexLoc })
                        (replaceExpr kroneckerLoc (Const 1) e) 
      else e


eliminateKronecker :: Expr -> Expr
eliminateKronecker e = let
  e'  = uniqueDummies_ $ expand e
  ts  = terms e'
  e'' = map (applyUntilStable eliminateOneKronecker) ts
  in
   canonicalize (Sum e'')
   
   
eliminateOneKroneckerTrace :: Expr -> Expr
eliminateOneKroneckerTrace e =  let
  kroneckerIndices     = findExpr' isContractedKronecker e
  kroneckerLoc         = snd $ fromJust kroneckerIndices
  kroneckerManifold    = tensorManifold $ fst $ fromJust kroneckerIndices
  kroneckerTrace       = manifoldDimension kroneckerManifold
  in
   if isJust kroneckerIndices
      then replaceExpr kroneckerLoc (Const (I (fromIntegral kroneckerTrace))) e
      else e


eliminateKroneckerTrace :: Expr -> Expr
eliminateKroneckerTrace e = let
  e'  = uniqueDummies_ $ expand e
  ts  = terms e'
  e'' = map (applyUntilStable eliminateOneKroneckerTrace) ts
  in
   canonicalize (Sum e'')
   
  
applyUntilStable :: Eq a => (a -> a) -> a -> a
applyUntilStable f x
  | x == y    = x
  | otherwise = applyUntilStable f y
  where
    y = f x
