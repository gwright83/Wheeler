--
-- TensorUtilities.hs
--
-- Functions to make defining tensor expressions easier.
--
-- Gregory Wright, 30 August 2011
--

module Math.Symbolic.Wheeler.TensorUtilities where



import Math.Symbolic.Wheeler.Tensor


mkIndex :: Manifold -> String -> VarIndex
mkIndex m n = Contravariant $ Abstract $ Index { indexManifold = m
                                               , indexName     = n
                                               , indexTeXName  = n }

mkIndex_ :: Manifold -> String -> String -> VarIndex
mkIndex_ m n texn = Contravariant $ Abstract $ Index { indexManifold = m
                                                     , indexName     = n
                                                     , indexTeXName  = texn }

getIndex :: VarIndex -> Index
getIndex (Covariant     (Abstract  s)) = s
getIndex (Covariant     (Component _)) = error "getIndex called on non-abstract index"
getIndex (Contravariant (Abstract  s)) = s
getIndex (Contravariant (Component _)) = error "getIndex called on non-abstract index"

