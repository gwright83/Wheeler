--
-- Pattern.hs
--
-- A place for functions related to pattern expressions.
--
-- Gregory Wright, 12 September 2012
--

module Math.Symbolic.Wheeler.Pattern where


import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.DiracSpinor
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.SimpleSymbol
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor


data PatternVar = SPat String
                | TPat String
                | DPat String
                | IPat String
                deriving (Eq, Ord, Show)
                       

isPattern :: Expr -> Bool
isPattern (Symbol (Simple s))      = simpleType s      == Pattern
isPattern (Symbol (DiracSpinor d)) = diracSpinorType d == Pattern
isPattern (Symbol (Tensor t))      = tensorType t      == Pattern
isPattern _ = False

getPattern :: Expr -> PatternVar
getPattern (Symbol (Simple s))      = SPat (simpleName s)
getPattern (Symbol (DiracSpinor d)) = DPat (diracSpinorName d)
getPattern (Symbol (Tensor t))      = TPat (tensorName t)
getPattern _ = error "getPattern called on non-pattern expression"


hasVarIndexPattern :: Expr -> Bool
hasVarIndexPattern (Symbol (Tensor t)) = any isPatternVarIndex (slots t)
hasVarIndexPattern _ = False

getVarIndexPattern :: VarIndex -> PatternVar
getVarIndexPattern (Covariant     (Abstract i)) = IPat (indexName i)
getVarIndexPattern (Contravariant (Abstract i)) = IPat (indexName i)
getVarIndexPattern (Covariant     (Component _)) = error "getVarIndexPattern of component index"
getVarIndexPattern (Contravariant (Component _)) = error "getVarIndexPattern of component index"

isPatternOrHasVarIndexPattern :: Expr -> Bool
isPatternOrHasVarIndexPattern e = isPattern e || hasVarIndexPattern e
