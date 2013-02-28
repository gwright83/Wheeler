--
-- Canonicalize.hs-boot
--


module Math.Symbolic.Wheeler.Canonicalize where

import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr

canonicalize :: Expr -> Expr
groupFactors :: [ Expr ] -> [ ([ String], [ Expr ]) ]
