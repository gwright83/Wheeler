--
-- FactorOut.hs
--
-- Factor something out of an expression.
--
-- Gregory Wright, 6 June 2012
--

module Math.Symbolic.Wheeler.FactorOut where


import Data.Maybe

import Math.Symbolic.Wheeler.Canonicalize
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.MetricTensor
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities



mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f (Sum ts)     = f (Sum (map f ts))
mapExpr f (Product fs) = f (Product (map f fs))
mapExpr f e = f e


containsTensor :: Expr -> Expr -> Bool
containsTensor t e = isJust $ findExpr (matchUpToVariance t) e


factorOutTensor :: Expr -> Expr -> Expr
factorOutTensor t e@(Sum ts) =
  let
    allTermsHaveTensor = all (containsTensor t) ts
    t' = dummyize t
  in
   if allTermsHaveTensor
      then canonicalize $ t' * (Sum (map (replaceWithKroneckers t t') ts))
      else e
    
factorOutTensor _ e = e


replaceWithKroneckers :: Expr -> Expr -> Expr -> Expr
replaceWithKroneckers t@(Symbol (Tensor tt)) t' e =
  let
    (t'', bc)   = fromJust $ findExpr' (matchUpToVariance t) e
    m           = manifold tt
    d           = \ x y -> if sameVariance x y
                           then mkKroneckerDelta_ m "delta" "\\delta" (-x) y
                           else mkNamedMetric m "g" "g" (-x) y
    replacement = Product $ zipWith d (tensorIndices t') (tensorIndices t'')
  in
    replaceExpr bc replacement e
    
replaceWithKroneckers _ _ e = e    
