--
-- Wheeler.hs
--
-- A top level module for loading test cases.
--
-- Gregory Wright, 18 June 2011
--


module Math.Symbolic.Wheeler where

import Data.Maybe

import Math.Symbolic.Wheeler.Basic
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Numeric
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities

a :: Expr
a = Symbol $ simpleSymbol "a"

b :: Expr
b = Symbol $ simpleSymbol "b"

c :: Expr
c = Symbol $ simpleSymbol "c"

e :: Expr
e = Symbol $ simpleSymbol "e"

l :: Expr
l = Symbol $ ncSymbol "l" "foo"
m :: Expr
m = Symbol $ ncSymbol "m" "foo"
n:: Expr
n = Symbol $ ncSymbol "n" "foo"

x :: Expr
x = Symbol $ ncSymbol "x" "pauli"
y :: Expr
y = Symbol $ ncSymbol "y" "pauli"

z:: Expr
z = Symbol $ ncSymbol "z" "pauli"

s :: Manifold
unnamedMetric :: Maybe NamedTensorExpr_2
(s, unnamedMetric) = mkManifold "s" 4 minkowski

mu :: VarIndex
mu    = mkIndex  s "mu"
nu :: VarIndex
nu    = mkIndex_ s "nu" "\\nu"
rho :: VarIndex
rho   = mkIndex s "delta"
sigma :: VarIndex
sigma = mkIndex s "sigma"

g :: VarIndex -> VarIndex -> Expr
g = (fromJust unnamedMetric) "g" "g"

g' :: Expr
g'   = g mu (-nu) * g (-sigma) rho * d (-rho) sigma
g'' :: Expr
g''  = g mu (-nu) * g (-rho) sigma * d (-mu) sigma
g''' :: Expr
g''' = g mu (-nu) * g (-mu) sigma  * d (-mu) rho


d :: VarIndex -> VarIndex -> Expr
d = mkKroneckerDelta s "delta"


foo :: Expr -> Expr
foo (Const (I k)) = if k == 1 then Const 0 else Const 2
foo _ = Const 3

fct :: Expr -> Expr
fct (Const (I 1)) = Const 1
fct i@(Const (I k)) = if k > 1 then i * fct (i - 1) else Const 1
fct _ = error "fct applied to non-integer"

hermite :: Integer -> Expr -> Expr
hermite k u =
    let
        herm 0 _ = 1 :: Expr
        herm 1  v = 2 * v
        herm k' v = 2 * v * herm (k' - 1) v - 2 * (k' - 1) * herm (k' - 2) v
    in
        expand $ herm (fromInteger k) u

main :: IO ()
main = do
    print $ hermite 10 (y+x)
