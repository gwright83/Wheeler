--
-- DiracSpinor.hs
--
-- Definitions for Dirac spinors.
--
-- Gregory Wright, 28 December 2011
--

module Math.Symbolic.Wheeler.DiracSpinor (
    D (..),
    diracConjugate,
    diracGamma_,
    diracSpinor_,
    diracSlash_,
    isConjugated
) where


import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Complexity
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Minkowski
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorSymmetries
import Math.Symbolic.Wheeler.TensorUtilities
import Math.Symbolic.Wheeler.UniqueID

import System.IO.Unsafe


data Conjugacy = Unconjugated | Conjugated deriving Eq

data D = D { diracIdentifier     :: Id
           , diracSpinorName     :: String
           , diracSpinorTeXName  :: String
           , diracSpinorMomentum :: VarIndex -> Expr
           , diracConjugacy      :: Conjugacy
           , diracCommutativity  :: Commutativity
           , diracSpinorType     :: SymbolType
           }

instance Eq D where
    (==) x y = diracSpinorName x         == diracSpinorName y &&
               diracConjugacy x          == diracConjugacy y  &&
               (diracSpinorMomentum x) i == (diracSpinorMomentum y) i
               where
                 (mf, _) = mkManifold "minkowski" 4 minkowski
                 i = mkIndex mf "i"

instance Ord D where
    compare _ _ = GT

instance Named D where
    name    = diracSpinorName
    teXName = diracSpinorTeXName

instance Identified D where
    identifier = diracIdentifier

instance Show D where
    showsPrec _ s = if isConjugated s
                       then showString "(diracConjugate "  .
                            showString (diracSpinorName s) .
                            showString ")"
                       else showString (diracSpinorName s)

instance Commutable D where
    commutativity = diracCommutativity


diracConjugate :: Expr -> Expr
diracConjugate (Symbol (DiracSpinor s)) =
    if diracConjugacy s == Conjugated
        then Symbol $ DiracSpinor $ s { diracConjugacy = Unconjugated }
        else Symbol $ DiracSpinor $ s { diracConjugacy = Conjugated }
diracConjugate _ = error "diracConjugate applied to non-diracSpinor"


isConjugated :: D -> Bool
isConjugated s = diracConjugacy s == Conjugated


-- A Dirac gamma matrix in four dimensions.
--
diracGamma_ :: RepSpace -> VarIndex -> Expr
diracGamma_ r i = 
    let
        indices = [ i ]
    in
        if  checkIndicesInManifold minkowskiManifold indices
            then
                unsafePerformIO $ do
                    ident <- nextId
                    return $ Symbol $ Tensor T { tensorIdentifier    = ident
                                               , tensorName          = "gamma"
                                               , tensorTeXName       = "\\gamma"
                                               , manifold            = minkowskiManifold
                                               , tensorClass         = General
                                               , tensorType          = Regular
                                               , slots               = indices
                                               , symmetry            = oneIndex
                                               , tensorComplexity    = Hermitian
                                               , tensorCommutativity = NonCommuting r
                                               , components          = Nothing }
            else error ("index error defining gamma matrix")

 
diracSpinor_ :: RepSpace -> String -> (VarIndex -> Expr) -> Expr
diracSpinor_ r nam v =
    unsafePerformIO $ do
        ident <- nextId
        return $ Symbol $ DiracSpinor D { diracIdentifier     = ident
                                        , diracSpinorName     = nam
                                        , diracSpinorTeXName  = nam
                                        , diracSpinorMomentum = v
                                        , diracConjugacy      = Unconjugated
                                        , diracCommutativity  = NonCommuting r 
                                        , diracSpinorType     = Regular }


diracSlash_ :: RepSpace -> (VarIndex -> Expr) -> Expr
diracSlash_ r v =
    let
        mu = mkIndex_ minkowskiManifold "mu" "\\mu"
        g  = diracGamma_ r
    in
        g mu * v (-mu)
