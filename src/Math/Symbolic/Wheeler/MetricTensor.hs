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
import Math.Symbolic.Wheeler.DummyIndices
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities



hasDummies :: Expr -> Bool
hasDummies (Symbol (Tensor t)) = any isDummy $ slots t
hasDummies _ = False


metricDummyIndices :: Expr -> [ IndexList ]
metricDummyIndices e = fi [] [] e
    where
        fi cxt il t@(Symbol (Tensor t'))  = if isMetric t && hasDummies t
                                             then (zipWith (\n i -> (i, Tcxt n : cxt) ) [1..] (slots t')) : il
                                             else il
        fi cxt il (Product ps) = concatMap (\(n, x) -> fi ((Pcxt n) : cxt) il x) $ zip [1..] ps
        fi cxt il (Sum  ts)    = concatMap (\(n, x) -> fi ((Scxt n) : cxt) il x) $ zip [1..] ts
        fi _   il _            = il


metricDummyIndices_ :: [ IndexList ] -> IndexList
metricDummyIndices_ il = map toEdit il
    where
      toEdit (i : i' : []) = if not (isDummy (fst i)) then (fst i, snd i') else (fst i', snd i)
      toEdit _             = error "metricDummyIndices_ applied to ill-formed list"
      

metricDummyEdits :: Expr -> IndexList
metricDummyEdits = metricDummyIndices_ . metricDummyIndices

findDummyMetric :: Expr -> Maybe Expr
findDummyMetric = findExpr isDummyMetric


isDummyMetric :: Expr -> Bool
isDummyMetric t@(Symbol (Tensor t')) = isMetric t   &&
                                       hasDummies t &&
                                       not (contractedPair (slots t'))
isDummyMetric _ = False
  

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


replaceExpr :: Breadcrumbs -> Expr -> Expr -> Expr
replaceExpr bc e' e = snd $ re bc e' ([], e)
    where
      re targetLoc rexpr (currentLoc, expr) = if currentLoc == targetLoc
                                                  then (currentLoc, rexpr)
                                                  else re' targetLoc rexpr (currentLoc, expr)
      re' targetLoc rexpr (currentLoc, Sum ts)     = (currentLoc, Sum     (zipWith (\n x -> snd (re targetLoc rexpr ((Scxt n) : currentLoc, x))) [1..] ts))
      re' targetLoc rexpr (currentLoc, Product fs) = (currentLoc, Product (zipWith (\n x -> snd (re targetLoc rexpr ((Pcxt n) : currentLoc, x))) [1..] fs))
      re' _ _ u@(_, _) = u
  

eliminateOneMetric :: Expr -> Expr
eliminateOneMetric e =  let
  e'                   = uniqueDummies_ $ expand e
  metricIndices        = findExpr' isDummyMetric e'
  (mi: mi' : [])       = tensorIndices $ fst $ fromJust metricIndices
  metricLoc            = snd $ fromJust metricIndices
  exprIndices          = concat $ collectIndices_ e'
  (oldIndex, newIndex) = if isDummy mi then ((-mi), mi') else ((-mi'), mi)
  indexLoc             = find (\x -> fst x == oldIndex) exprIndices
  in
   if isJust metricIndices && isJust indexLoc
      then replaceIndex (newIndex, snd $ fromJust indexLoc) (replaceExpr metricLoc (Const 1) e') 
      else e

  
applyUntilStable :: Eq a => (a -> a) -> a -> a
applyUntilStable f x
  | x == y = x
  | otherwise = applyUntilStable f y
  where
    y = f x
