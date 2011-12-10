{-# LANGUAGE FlexibleContexts #-}
--
-- NumericRational.hs
--
-- Simplify rational expressions
--
-- Gregory Wright, 3 May 2011
--
-- Modified to use the new annotated expression type,
-- which changes all of the function type signatures.
--

module Math.Symbolic.Wheeler.NumericRational where


import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Numeric


simplifyRNE :: Expr -> Expr
simplifyRNE e =
    let
        v = simplifyRNE' e
    in
        if v == Undefined then Undefined else simplifyRationalNumber v


simplifyRationalNumber :: Expr -> Expr
simplifyRationalNumber i@(Const (I _)) = i
simplifyRationalNumber (Const (Q n d))
    | n `rem` d == 0 = Const (I (n `div` d))
    | otherwise      = let g = gcd n d in
                       if d > 0 
                           then Const (Q (n  `div` g) (d  `div` g))
                           else Const (Q (-n `div` g) (-d `div` g))
simplifyRationalNumber _ = error "simplifyRationalNumber applied to non-rational"


simplifyRNE' :: Expr -> Expr
simplifyRNE' Undefined = Undefined
simplifyRNE' i@(Const (I _)) = i
simplifyRNE' q@(Const (Q _ d))
    | d == 0 = Undefined
    | otherwise = q
simplifyRNE' (Sum (t : t' : [])) =
    let
        v = simplifyRNE' t
        w = simplifyRNE' t'
    in
        if v == Undefined || w == Undefined
            then Undefined
            else evaluateSum v w
simplifyRNE' (Product (f : f' : [])) =
    let
        v = simplifyRNE' f
        w = simplifyRNE' f'
    in
        if v == Undefined || w == Undefined
            then Undefined
            else evaluateProduct v w
simplifyRNE' (Power b e) =
    let
        v = simplifyRNE' b
    in
        if v == Undefined
            then Undefined
            else evaluatePower v e
simplifyRNE' _ = error "simplifyRNE' applied to non-rational"


evaluateSum :: Expr -> Expr -> Expr
evaluateSum (Const t) (Const t') = Const (t + t')
evaluateSum _ _ = error "evaluateSum applied to non-constants"


evaluateProduct :: Expr -> Expr -> Expr
evaluateProduct (Const f) (Const f') = Const (f * f')
evaluateProduct _ _ = error "evaluateProduct applied to non-constants"


evaluateQuotient ::Expr -> Expr -> Expr
evaluateQuotient Undefined _ = Undefined
evaluateQuotient _ Undefined = Undefined
evaluateQuotient (Const n) (Const d) = 
    if denominator quotient == 0 then Undefined else (Const quotient)
    where
        quotient = n / d
        denominator (I _)       = 1
        denominator (Q _ denom) = denom
evaluateQuotient _ _ = error "evaluateQuotient applied to non-constants"


 
evaluatePower :: Expr -> Expr -> Expr
evaluatePower b@(Const v) (Const (I n))
    | numerator v == 0 && n >= 1 = Const (I 0)
    | numerator v == 0 && n <= 0 = Undefined
    | n > 0                      = let s = evaluatePower b (Const (I (n - 1))) in evaluateProduct s b
    | n == 0                     = Const (I 1)
    | n == -1                    = Const (1 / v)
    | otherwise                  = evaluatePower (Const (1 / v)) (Const (I (-n)))
    where
        numerator (I num)   = num
        numerator (Q num _) = num
evaluatePower _ _ = error "evaluatePower applied to non-constants"

