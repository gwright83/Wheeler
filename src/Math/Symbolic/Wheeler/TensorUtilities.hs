--
-- TensorUtilities.hs
--
-- Functions to make defining tensor expressions easier.
--
-- Gregory Wright, 30 August 2011
--

module Math.Symbolic.Wheeler.TensorUtilities where


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