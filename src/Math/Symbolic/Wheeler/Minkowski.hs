--
-- Minkowski.hs
--
-- The meanifold and metric of Minkowski space.
--
-- Gregory Wright, 13 January 2012
--

module Math.Symbolic.Wheeler.Minkowski where

import Data.Maybe

import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities



type TensorExpr = VarIndex -> VarIndex -> Expr

minkowskiM :: (Manifold, Maybe NamedTensorExpr_2)
minkowskiM = mkManifold "minkowski" 4 minkowski

minkowskiManifold :: Manifold
minkowskiManifold = fst minkowskiM

unnamedMinkowskiMetric :: Maybe NamedTensorExpr_2
unnamedMinkowskiMetric = snd minkowskiM

minkowskiMetric :: String -> TensorExpr
minkowskiMetric nam = (fromJust unnamedMinkowskiMetric) nam nam

minkowskiIndex_ :: String -> String -> VarIndex
minkowskiIndex_ = mkIndex_ minkowskiManifold



