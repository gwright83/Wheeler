--
-- TensorComponents.hs
--
-- Gregory Wright, 26 August 2011
--

module Math.Symbolic.Wheeler.TensorComponents (
    TensorComponents (..)
) where

import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr

data TensorComponents = TensorComponents [ Expr ]

