{-# OPTIONS_GHC -XGADTs #-}
--
-- Expr.hs-boot
--
-- Break module import cycle
--

module Math.Symbolic.Wheeler.Expr where

import Math.Symbolic.Wheeler.Numeric
import Math.Symbolic.Wheeler.Function
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.SumOrd

data Expr where
    Const     :: Numeric -> Expr
    Applic    :: Function -> Expr -> Expr
    Symbol    :: Symbol -> Expr
    Sum       :: [ Expr ] -> Expr
    Product   :: [ Expr ] -> Expr
    Power     :: Expr -> Expr -> Expr
    Undefined :: Expr

instance Eq Expr
instance Show Expr
instance Ord Expr
instance SumOrd Expr
instance Num Expr
instance Fractional Expr
instance Floating Expr

repSpaces :: Expr -> [ String ]


