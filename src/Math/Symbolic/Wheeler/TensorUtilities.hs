--
-- TensorUtilities.hs
--
-- Functions to make defining tensor expressions easier.
--
-- Gregory Wright, 30 August 2011
--

module Math.Symbolic.Wheeler.TensorUtilities where


import System.IO.Unsafe

import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.UniqueID


mkIndex :: Manifold -> String -> VarIndex
mkIndex m n = Contravariant $ Abstract $ Index { indexManifold = m
                                               , indexName     = n
                                               , indexTeXName  = n
                                               , indexType     = Regular }

mkIndex_ :: Manifold -> String -> String -> VarIndex
mkIndex_ m n texn = Contravariant $ Abstract $ Index { indexManifold = m
                                                     , indexName     = n
                                                     , indexTeXName  = texn
                                                     , indexType     = Regular }

mkPatternIndex :: VarIndex
mkPatternIndex = Contravariant $ Abstract $ Index { indexManifold = emptyManifold
                                                  , indexName = ""
                                                  , indexTeXName = ""
                                                  , indexType = Pattern }
                   
emptyManifold :: Manifold
emptyManifold =  Manifold { manifoldType           = PatternManifold 
                          , manifoldName           = ""
                          , manifoldTeXName        = ""
                          , manifoldDimension      = 0
                          , manifoldDimensionDelta = Nothing
                          , metricity              = NoMetric }
               
               
getIndex :: VarIndex -> Index
getIndex (Covariant     (Abstract  s)) = s
getIndex (Covariant     (Component _)) = error "getIndex called on non-abstract index"
getIndex (Contravariant (Abstract  s)) = s
getIndex (Contravariant (Component _)) = error "getIndex called on non-abstract index"


-- uniqueDummy produces a contravariant index with a
-- unique name and marked as an explicit dummy.
--
uniqueDummy :: Manifold -> IO VarIndex
uniqueDummy m = do
    n <- nextDummy
    return $ Contravariant $ Abstract $ Index { indexManifold = m
                                              , indexName     = show n
                                              , indexTeXName  = show n
                                              , indexType     = ExplicitDummy }


isCovariant :: VarIndex -> Bool
isCovariant (Covariant _) = True
isCovariant _             = False

isContravariant :: VarIndex -> Bool
isContravariant (Contravariant _) = True
isContravariant _                 = False

toCovariant :: VarIndex -> VarIndex
toCovariant (Contravariant i) = Covariant i
toCovariant (Covariant     i) = Covariant i

toContravariant :: VarIndex -> VarIndex
toContravariant (Contravariant i) = Contravariant i
toContravariant (Covariant     i) = Contravariant i

isDummy :: VarIndex -> Bool
isDummy (Covariant     (Abstract i)) = indexType i == ExplicitDummy
isDummy (Contravariant (Abstract i)) = indexType i == ExplicitDummy
isDummy _ = False


-- is a tensor a metric, Kronecker delta or Levi-Civita symbol?
--
isMetric :: Expr -> Bool
isMetric (Symbol (Tensor t)) = tensorType t == Metric
isMetric _ = False

isKroneckerDelta :: Expr -> Bool
isKroneckerDelta (Symbol (Tensor t)) = tensorType t == KroneckerDelta
isKroneckerDelta _ = False

isLeviCivita :: Expr -> Bool
isLeviCivita (Symbol (Tensor t)) = tensorType t == LeviCivita
isLeviCivita _ = False

tensorIndices :: Expr -> [ VarIndex ]
tensorIndices (Symbol (Tensor t)) = slots t
tensorIndices _ = []

tensorManifold :: Expr -> Manifold
tensorManifold (Symbol (Tensor t)) = manifold t
tensorManifold _ = error "tensorManifold applied to non-tensor expression"


-- Take a tensor and replace its indices by dummies
--
dummyize :: Expr -> Expr
dummyize (Symbol (Tensor t)) = Symbol $ Tensor $ t { slots = t' }
  where
    m  = manifold t
    t' = map (\i -> if isContravariant i
                        then unsafePerformIO $ uniqueDummy m
                        else - (unsafePerformIO $ uniqueDummy m)) $ slots t
         
dummyize e = e

