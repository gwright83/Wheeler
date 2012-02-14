--
-- Canonicalize.hs
--
-- Transform an expression into canonical form.  This is essentially
-- a simplification, but the real goal is that identical expressions
-- will have identical representations after canonicalization.
--
-- Gregory Wright, 27 April 2011
--

module Math.Symbolic.Wheeler.Canonicalize (
       canonicalize,
       simplifySum,
       simplifyProduct,
       simplifyFactors,
       groupFactors,
       mergeCommutingFactors,
       mergeNoncommutingFactors,
       findCorrespondingFactors,
       groupExprs
) where


import Data.List
import Data.Maybe
import Data.Ratio
import Debug.Trace

import Math.Symbolic.Wheeler.Debug
import {-# SOURCE #-} Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Numeric
import Math.Symbolic.Wheeler.NumericRational
import Math.Symbolic.Wheeler.SumOrd


canonicalize :: Expr -> Expr
canonicalize c@(Const _)   = simplifyConstant c
canonicalize s@(Symbol _)  = s
canonicalize (Product x)   = traceCall "simplifyProduct" simplifyProduct (Product (map canonicalize x))
canonicalize (Power b e)   = simplifyPower (Power (canonicalize b) (canonicalize e))
canonicalize (Sum x)       = simplifySum (Sum (map canonicalize x))
canonicalize (Applic f a)  = simplifyFunction (Applic f (canonicalize a))
canonicalize Undefined     = Undefined


-- Test if a constant expression is numerically zero.  Note that the expression
-- is not evaluated; the only thing that is checked is if it is a constant,
-- and the constant is zero.

isConstantZero :: Expr -> Bool
isConstantZero (Const (I 0))   = True
isConstantZero (Const (Q _ 0)) = False
isConstantZero (Const (Q 0 _)) = True
isConstantZero _               = False

isDivideByZero :: Expr -> Bool
isDivideByZero (Const (Q _ 0)) = True
isDivideByZero _               = False

isConstantOne :: Expr -> Bool
isConstantOne (Const (I 1)) = True
isConstantOne _             = False

isConstant :: Expr -> Bool
isConstant (Const _) = True
isConstant _         = False

isConstantInteger :: Expr -> Bool
isConstantInteger (Const (I _)) = True
isConstantInteger _             = False

isPositive :: Expr -> Bool
isPositive (Const (I n))   = n > 0
isPositive (Const (Q n d)) = (n % d) > 0
isPositive _               = False


-- simplifyConstant checks rationals for explicit division
-- by zero, otherwise it returns the argument unchanged.

simplifyConstant :: Expr -> Expr
simplifyConstant q@(Const (Q _ _)) = simplifyRNE q
simplifyConstant i@(Const (I _))   = i
simplifyConstant _                 = error "simplifyConstant applied to non-constant expression"


-- 'simplifyProduct' implements the transformations given in Definition 3.37
-- of Cohen's "Computer Algebra and Symbolic Computation: Mathematical Methods",
-- p. 98.

simplifyProduct :: Expr -> Expr
simplifyProduct (Product fs) = sp (fs)
    where
        sp fs'
            | null fs'                = Const 1
            | elem Undefined fs'      = Undefined
            | any isDivideByZero fs'  = Undefined
            | elem (Const 0) fs'      = Const 0
            | null (tail fs')         = head fs'
            | otherwise               =
                let
                    simplifyProduct' :: [ Expr ] -> Expr
                    simplifyProduct' []       = Const 1
                    simplifyProduct' (x : []) = x
                    simplifyProduct' xs       = Product xs
                in
                    traceCall "simplifyProduct'" simplifyProduct' (traceCall "simplifyFactors" simplifyFactors fs')
simplifyProduct _ = error "simplifyProduct applied to non-product expression"
    

simplifyFactors :: [ Expr ] -> [ Expr ]
simplifyFactors []                              = []
simplifyFactors (Product fs : Product fs' : []) = traceCall2 "mergeFactors (1)" mergeFactors fs  fs'
simplifyFactors (Product fs : u : [])           = traceCall2 "mergeFactors (2)" mergeFactors fs [ u ]
simplifyFactors (u : Product fs : [])           = traceCall2 "mergeFactors (3)" mergeFactors [u] fs
simplifyFactors p@(Const (I n) : Sum ts : [])
    | n == -1   =
        let
            negate' e = Product [Const (-1), e]
        in
            [simplifySum (Sum (map negate' ts))]
    | otherwise = p
simplifyFactors (u1 : u2 : [])
    | isConstant u1 && isConstant u2 =
        let
            p = simplifyRNE (Product [u1,u2])
        in
            if p == (Const 1) then [] else [p]
    | isConstantOne u1 = [u2]
    | isConstantOne u2 = [u1]
    | basePart u1 == basePart u2 =
        let
            s = simplifySum (Sum [exponentPart u1, exponentPart u2])
            p = simplifyPower (Power (basePart u1) s)
        in
            if p == (Const 1) then [] else [p]
    | u2 < u1   = [u2, u1]
    | otherwise = [u1, u2]
simplifyFactors (u : us) =
    let
        w = simplifyFactors us

        factorList :: Expr -> [ Expr ]
        factorList (Product vs) = vs
        factorList x = [ x ]
    in
        traceCall2 "mergeFactors (4)" mergeFactors (factorList u) w


mergeFactors :: [ Expr ] -> [ Expr ] -> [ Expr ]
mergeFactors [] [] = []
mergeFactors  p [] = p
mergeFactors []  q = q
mergeFactors p q =
    let
        p' = groupFactors p
        q' = groupFactors q
        f  = traceCall2 "findCorrespondingFactors" findCorrespondingFactors p' q'

        c = lookup [] f
        n = deleteBy (\(x, (_, _)) (y, (_, _)) -> x == y) ([], (undefined, undefined)) f
        (_, (c', c'')) = if isJust c then ([], fromJust c) else ([], ([],[]))
        mergedCommuting = mergeCommutingFactors c' c''

        mergedNoncommuting = map (\(_, (y, z)) -> traceCall2 "mergeNoncommutingFactors" mergeNoncommutingFactors y z) n
    in
        mergedCommuting ++ concat ((\x -> trace ("mergedNonCommuting " ++ show x) x) mergedNoncommuting)


-- This is the orginal definition of factor merge for commuting factors:
--
-- It depends on the argument expression lists being "admissible"
-- factors, which means that they can contain constants, symbols,
-- sums, powers or functions but not products.
--
mergeCommutingFactors :: [ Expr ] -> [ Expr ] -> [ Expr ]
mergeCommutingFactors [] [] = []
mergeCommutingFactors []  q = q
mergeCommutingFactors p  [] = p
mergeCommutingFactors pp@(p : ps) qq@(q : qs) =
    let
        h = simplifyFactors [p, q]
    in
        if null h
            then mergeCommutingFactors ps qs
            else if null (tail h)
                    then (head h) : mergeCommutingFactors ps qs
                    else if head h == p
                            then p : mergeCommutingFactors ps qq
                            else q : mergeCommutingFactors pp qs


-- This version of works for non-commutative
-- expressions:
--
mergeNoncommutingFactors :: [ Expr ] -> [ Expr ] -> [ Expr ]
mergeNoncommutingFactors [] [] = []
mergeNoncommutingFactors []  q = q
mergeNoncommutingFactors p  [] = p
mergeNoncommutingFactors p   q =
    let
        ps = init p
        p' = last p
        q' = head q
        qs = tail q

        h = simplifyFactors [p', q']
    in
        if null h
            then mergeNoncommutingFactors ps qs
            else if null (tail h)
                    then mergeNoncommutingFactors ps ((head h) : qs)
                    else if head h == p'
                            then mergeNoncommutingFactors ps (p' : q)
                            else mergeNoncommutingFactors ps (q' : p' : qs)


-- XXX FIXME XXX
-- This function doesn't work properly.
--
findCorrespondingFactors :: [ ([ String ], [ Expr ]) ]
                  -> [ ([ String ], [ Expr ]) ]
                  -> [ ([ String ], ([ Expr ], [ Expr ])) ]
findCorrespondingFactors [] [] = []
findCorrespondingFactors  p [] = map (\(x, y) -> (x, (y, []))) p
findCorrespondingFactors []  q = map (\(x, y) -> (x, ([], y))) q
findCorrespondingFactors (p : ps) qs =
    let
        c = lookup (fst p) qs
        qs' = if isJust c
                  then deleteBy (\(x, _) (y, _) -> x == y) (fst p, undefined) qs
                  else qs
    in
        if isJust c
            then (\(x, y) z -> (x, (y, z ))) p (fromJust c) : findCorrespondingFactors ps qs'
            else (\(x, y)   -> (x, (y, []))) p              : findCorrespondingFactors ps qs'


-- Group the factors in an expression list by representation
-- space.  The return value is a list of the names of the representation
-- spaces and the expressions that live on those spaces.  Ordinary
-- commuting factors live in the anonymous space, whose name
-- is the empty list.
--
-- If there are no commuting factors, prefix a pair
-- of empty lists to indicate this.
--
groupFactors :: [ Expr ] -> [ ([ String ], [ Expr ]) ]
groupFactors es =
    let
        es'  = groupExprs es
        es'' = map (\x -> (repSpaces (head x), x)) es'
    in
        if isJust (lookup [] es'') then es'' else ([], []) : es''


groupExprs :: [ Expr ] -> [[ Expr ]]
groupExprs [] = []
groupExprs pp@(p : _) =
    let
        (p', q') = partition (\x -> repSpaces x == repSpaces p) pp
    in
        p' : groupExprs q' 


basePart :: Expr -> Expr
basePart (Const _)   = Undefined
basePart (Power b _) = b
basePart x           = x

exponentPart :: Expr -> Expr
exponentPart (Const _)   = Undefined
exponentPart (Power _ e) = e
exponentPart _           = Const (I 1)


-- Simplify powers:

simplifyPower :: Expr -> Expr
simplifyPower p@(Power b e)
    | b == Undefined || e == Undefined = Undefined
    | isConstantZero b                 = if isPositive e then Const (I 0) else Undefined
    | isConstantOne b                  = Const (I 1)
    | isConstantInteger e              = simplifyIntegerPower p
    | otherwise                        = p
simplifyPower _ = error "simplifyPower applied to non-power"


-- 'simplifyIntegerPower' handles the case in which an expression is
-- raised to an integer power.

-- For products of non-commuting factors, there are still bugs.
-- The order of factors should be reversed only for negative integer
-- powers.  For any other integer power of non-commuting factors,
-- should I just leave the expression unchanged?
--
simplifyIntegerPower :: Expr -> Expr
simplifyIntegerPower (Power (Const (I b))   (Const (I e))) = simplifyRNE (Power (Const (I b))   (Const (I e)))
simplifyIntegerPower (Power (Const (Q n d)) (Const (I e))) = simplifyRNE (Power (Const (Q n d)) (Const (I e)))
simplifyIntegerPower (Power _ (Const (I 0)))               = Const 1
simplifyIntegerPower (Power b (Const (I 1)))               = b

simplifyIntegerPower (Power (Power r s) e@(Const (I _))) =
    let
        p = simplifyProduct (Product [s, e])
    in
        if isConstantInteger p
            then simplifyIntegerPower (Power r p)
            else (Power r p)

simplifyIntegerPower (Power (Product fs) e@(Const (I _))) =
    let
        fs'  = groupFactors fs
        cfs  = fromJust (lookup [] fs')    -- lookup will never return Nothing
                                           -- because a [] association is always    
                                           -- returned by groupFactors
        ncfs = deleteBy (\(x, _) (y, _) -> x == y) ([], undefined) fs'

        r  = if null cfs
                 then []
                 else map (\f -> simplifyIntegerPower (Power f e)) cfs

        r' = if null ncfs
                 then []
                 else [ (Power (Product (concatMap snd ncfs)) e) ]

        r'' = simplifyProduct (Product r)

        -- Unwrap a commuting product:

        commutingProduct (Const 1)    = []
        commutingProduct (Product cs) = cs
        commutingProduct cs           = [ cs ]
    in
        Product (commutingProduct r'' ++ r')
            
simplifyIntegerPower (Power b e@(Const (I _))) = Power b e
simplifyIntegerPower _ = error "simplifyIntegerPower applied to non-power"


--reverseNonCommutingFactors :: [ Expr ] -> [ Expr ]
--reverseNonCommutingFactors fs =
--    let
--        fs' = groupFactors fs
--        fs'' = map (\(x, y) -> if null x then (x, y) else (x, reverse y)) fs'
--    in
--        concatMap snd fs''


-- 'simplifySum' parallels 'simplyProduct'.  It is outlined but
-- not completely defined on p. 105 of Cohen.

simplifySum :: Expr -> Expr
simplifySum (Sum ts)
    | elem Undefined ts = Undefined
    | null (tail ts)         = head ts
    | otherwise              = simplifySum' (simplifyTerms ts)
    where
        simplifySum' :: [ Expr ] -> Expr
        simplifySum' []       = Const 0
        simplifySum' (t : []) = t
        simplifySum' tt       = Sum tt
simplifySum _ = error "simplifySum applied to non-sum"


simplifyTerms :: [ Expr ] -> [ Expr ]
simplifyTerms []                      = []
simplifyTerms (Sum ts : Sum ts' : []) = mergeTerms ts ts'
simplifyTerms (Sum ts : u : [])       = mergeTerms ts [u]
simplifyTerms (u : Sum ts : [])       = mergeTerms [u] ts
simplifyTerms (u1 : u2 : [])
    | isConstant u1 && isConstant u2 =
        let
            p = simplifyRNE (Sum [u1, u2])
        in
            if p == (Const 0) then [] else [p]
    | isConstantZero u1 = [u2]
    | isConstantZero u2 = [u1]
    | termPart u1 == termPart u2 =
        let
            s = simplifySum (Sum [constantPart u1, constantPart u2])
            p = simplifyProduct (Product [termPart u1, s])
        in
            if p == Const 0 then [] else [p]
     | (sumCompare u2 u1) == LT = [u2, u1]
     | otherwise                = [u1, u2]
simplifyTerms (u : us) = 
    let
        w = simplifyTerms us

        termList :: Expr -> [ Expr ]
        termList (Sum vs) = vs
        termList x        = [ x ]
    in
        mergeTerms (termList u) w


mergeTerms :: [ Expr ] -> [ Expr ] -> [ Expr ]
mergeTerms [] [] = []
mergeTerms [] q  = q
mergeTerms p  [] = p
mergeTerms pp@(p : ps) qq@(q : qs) =
    let
        h = simplifyTerms [p, q]
    in
        if null h
            then mergeTerms ps qs
            else if null (tail h)
                    then (head h) : mergeTerms ps qs
                    else if head h == p
                              then p : mergeTerms ps qq
                              else q : mergeTerms pp qs


constantPart :: Expr -> Expr
constantPart (Const _)         = Undefined
constantPart (Symbol _)        = Const 1
constantPart (Sum _)           = Const 1
constantPart (Power _ _)       = Const 1
constantPart (Applic _ _)      = Const 1
constantPart (Product [])      = Undefined
constantPart (Product (u : _)) = if isConstant u then u else Const 1
constantPart Undefined         = Undefined

termPart :: Expr -> Expr
termPart (Const _)            = Undefined
termPart x@(Symbol _)         = Product [x]
termPart x@(Sum _)            = Product [x]
termPart x@(Power _ _)        = Product [x]
termPart x@(Applic _ _)       = Product [x]
termPart (Product [])         = Undefined
termPart x@(Product (u : us)) = if isConstant u then Product us else x
termPart Undefined            = Undefined



-- 'simplifyfunction' is the identity unless one of the
-- arguments is undefined, in which case the entire expression
-- in undefined:

simplifyFunction :: Expr -> Expr
simplifyFunction   (Applic _ Undefined) = Undefined
simplifyFunction g@(Applic _ _)         = g
simplifyFunction _                      = error "simplifyFunction applied to non-function"



