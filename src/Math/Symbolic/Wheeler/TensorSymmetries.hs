--
-- TensorSymmetries.hs
--
-- Types and convenience functions for specifying tensor
-- symmetries.
--
-- Gregory Wright, 7 September 2011
--

module Math.Symbolic.Wheeler.TensorSymmetries (
    Symmetry (..),
    oneIndex,
    symmetricTwoIndex,
    symmetricThreeIndex,
    symmetricFourIndex,
    antisymmetricTwoIndex,
    antisymmetricThreeIndex,
    antisymmetricFourIndex
) where


-- A tensor's symmetry group is specified by a base and
-- generating set.
--
data Symmetry = Symmetry Base GeneratingSet
    deriving Show

newtype Base = Base { base :: [ Int ] }
    deriving Show
newtype GeneratingSet = GeneratingSet { sgs :: [[ Int ]] }
    deriving Show

oneIndex :: Symmetry
oneIndex = Symmetry (Base {base = [1]}) (GeneratingSet {sgs = [[1, 2, 3]]})

symmetricTwoIndex:: Symmetry
symmetricTwoIndex   = Symmetry (Base {base = [1, 2]})       (GeneratingSet {sgs = [[1, 2, 3, 4]]})

symmetricThreeIndex :: Symmetry
symmetricThreeIndex = Symmetry (Base {base = [1, 2, 3]})    (GeneratingSet {sgs = [[1, 2, 3, 4, 5]]})

symmetricFourIndex :: Symmetry
symmetricFourIndex  = Symmetry (Base {base = [1, 2, 3, 4]}) (GeneratingSet {sgs = [[1, 2, 3, 4, 5, 6]]})

antisymmetricTwoIndex :: Symmetry
antisymmetricTwoIndex   = Symmetry (Base {base = [1, 2]})       (GeneratingSet {sgs = [[1, 2, 4, 3]]})

antisymmetricThreeIndex :: Symmetry
antisymmetricThreeIndex = Symmetry (Base {base = [1, 2, 3]})    (GeneratingSet {sgs = [[1, 2, 3, 5, 4]]})

antisymmetricFourIndex :: Symmetry
antisymmetricFourIndex  = Symmetry (Base {base = [1, 2, 3, 4]}) (GeneratingSet {sgs = [[1, 2, 3, 4, 6, 5]]})


