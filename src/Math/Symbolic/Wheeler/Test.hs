--
-- Test.hs
--
-- A top level module for loading test cases.
--
-- Gregory Wright, 18 June 2011
--


module Main where

import Data.Maybe
import Ratio

import Basic
import Canonicalize
import Commutativity
import Expr
import Expression
import Numeric
import Symbol
import IO
import Tensor
import TensorBasics
import TensorUtilities


a = Symbol $ simpleSymbol "a"
b = Symbol $ simpleSymbol "b"
c = Symbol $ simpleSymbol "c"
e = Symbol $ simpleSymbol "e"

l = Symbol $ ncSymbol "l" "foo"
m = Symbol $ ncSymbol "m" "foo"
n = Symbol $ ncSymbol "n" "foo"
  
x = Symbol $ ncSymbol "x" "pauli"
y = Symbol $ ncSymbol "y" "pauli"
z = Symbol $ ncSymbol "z" "pauli"

(s, unnamedMetric) = mkManifold "s" 4 minkowski

mu    = mkIndex  s "mu"
nu    = mkIndex_ s "nu" "\\nu"
rho   = mkIndex s "delta"
sigma = mkIndex s "sigma"

g = (fromJust unnamedMetric) "g" "g"

g'   = g mu (-nu) * g (-sigma) rho * d (-rho) sigma
g''  = g mu (-nu) * g (-rho) sigma * d (-mu) sigma
g''' = g mu (-nu) * g (-mu) sigma  * d (-mu) rho


d = mkKroneckerDelta s "delta"


foo :: Expr -> Expr
foo (Const (I n)) = if n == 1 then Const 0 else Const 2
foo _ = Const 3

fct :: Expr -> Expr
fct (Const (I 1)) = Const 1
fct i@(Const (I n)) = if n > 1 then i * fct (i - 1) else Const 1


hermite :: Integer -> Expr -> Expr
hermite n x =
    let
        herm 0 _ = 1 :: Expr
        herm 1 x = 2 * x
        herm n x = 2 * x * herm (n - 1) x - 2 * (n - 1) * herm (n - 2) x
    in
        expand $ herm (fromInteger n) x

main = do
    print $ hermite 10 (y+x)
