--
-- Matchable.hs
--
-- Definition for the Matchable class, to which belong
-- objects that can be matched, and may contain pattern
-- variables.
--
-- Gregory Wright, 10 September 2012
--

module Math.Symbolic.Wheeler.Matchable where


import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe


import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.DiracSpinor
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Pattern
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.SimpleSymbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities


           
data Binding = SVar S
             | TVar T
             | DVar D
             | IVar Index
             deriving (Eq, Ord, Show)

unBind :: Binding -> Expr
unBind (SVar s) = Symbol (Simple s)
unBind (TVar t) = Symbol (Tensor t)
unBind (DVar d) = Symbol (DiracSpinor d)
unBind _        = error "unBind error"

unBindIndex :: Binding -> Index
unBindIndex (IVar i) = i
unBindIndex _        = error "unBindIndex"


type NodeLocations = [ Breadcrumbs' ]

type Environ = (Map.Map PatternVar Binding, NodeLocations)
  

class Matchable a where
  leafMatch     :: a -> a -> State Environ Bool
  updateEnviron :: a -> a -> State Environ Bool
  updateEnviron _ _ = return False
  

instance Matchable S where
  leafMatch x y = do
    if (simpleType x == Pattern && simpleType y /= Pattern)
      then updateEnviron x y
      else if (simpleType x /= Pattern && simpleType y == Pattern)
           then updateEnviron y x
           else if (simpleType x == Pattern && simpleType y == Pattern)
                then return False
                else return $ ((simpleIdentifier x) == (simpleIdentifier y)) &&
                              ((simpleType x)       == (simpleType y))
                            
  updateEnviron pat subj = do
    (bindings, nl) <- get
    let
      patName = SPat (simpleName pat)
      v = Map.lookup patName bindings
    if isJust v
      then return (fromJust v == SVar subj)
      else do put (Map.insert patName (SVar subj) bindings, nl)
              return True
    
    
instance Matchable T where
  leafMatch x y = do
    if (tensorType x == Pattern && tensorType y /= Pattern)
      then do
           savedEnv     <- get
           indicesMatch <- zipWithM leafMatch (slots x) (slots y)
           if length (slots x) == length (slots y) && and indicesMatch
             then updateEnviron x y
             else do put savedEnv; return False
      else if (tensorType x /= Pattern && tensorType y == Pattern)
           then do
             savedEnv     <- get
             indicesMatch <- zipWithM leafMatch (slots x) (slots y)
             if length (slots x) == length (slots y) && and indicesMatch
               then updateEnviron x y
               else do put savedEnv; return False
           else if (tensorType x == Pattern && tensorType y == Pattern)
                then return False
                else do
                  savedEnv     <- get
                  indicesMatch <- zipWithM leafMatch (slots x) (slots y)
                  if length (slots x) == length (slots y) && and indicesMatch  
                    then return $ (manifold x   == manifold y)   &&
                                  (tensorName x == tensorName y)
                    else do put savedEnv; return False

  
  updateEnviron pat subj = do
    (bindings, nl) <- get
    let
      patName = TPat (tensorName pat)
      v = Map.lookup patName bindings
    if isJust v
      then return (fromJust v == TVar subj)
      else do put (Map.insert patName (TVar subj) bindings, nl)
              return True
    

instance Matchable D where
  leafMatch x y = do
    if (diracSpinorType x == Pattern && diracSpinorType y /= Pattern)
      then updateEnviron x y
      else if (diracSpinorType x /= Pattern && diracSpinorType y == Pattern)
           then updateEnviron y x
           else if (diracSpinorType x == Pattern && diracSpinorType y == Pattern)
                then return False
                else do
                  let
                    (mf, _) = mkManifold "minkowski" 4 minkowski
                    i = mkIndex mf "i"
                  return $ diracSpinorName x         == diracSpinorName y &&
                           diracConjugacy x          == diracConjugacy y  &&
                           (diracSpinorMomentum x) i == (diracSpinorMomentum y) i
  
  updateEnviron pat subj = do
    (bindings, nl) <- get
    let
      patName = DPat (diracSpinorName pat)
      v = Map.lookup patName bindings
    if isJust v
      then return (fromJust v == DVar subj)
      else do put (Map.insert patName (DVar subj) bindings, nl)
              return True
    
        
instance Matchable Index where
  leafMatch x y = do
    if (indexType x == Pattern && indexType y /= Pattern)
      then updateEnviron x y
      else if (indexType x /= Pattern && indexType y == Pattern)
           then updateEnviron y x
           else if (indexType x == Pattern && indexType y == Pattern)
                then return False
                else return $ ((indexManifold x) == (indexManifold y)) &&
                              ((indexName x)     == (indexName y))
  
  updateEnviron pat subj = do
    (bindings, nl) <- get
    let
      patName = IPat (indexName pat)
      v = Map.lookup patName bindings
    if isJust v
      then return (fromJust v == IVar subj)
      else do put (Map.insert patName (IVar subj) bindings, nl)
              return True
                            

instance Matchable VarIndex where
  leafMatch (Covariant     (Abstract  x)) (Covariant     (Abstract  y)) = leafMatch x y
  leafMatch (Contravariant (Abstract  x)) (Contravariant (Abstract  y)) = leafMatch x y
  leafMatch (Covariant     (Component x)) (Covariant     (Component y)) = return (x == y)
  leafMatch (Contravariant (Component x)) (Contravariant (Component y)) = return (x == y)
  leafMatch _ _ = return False
   

