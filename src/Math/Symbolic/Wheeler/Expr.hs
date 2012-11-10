{-# OPTIONS_GHC -XGADTs #-}
--
-- Expr.hs
--
-- The Expr datatype; the base module imported by anything
-- that operates on expressions. 
--
-- Gregory Wright, 7 March 2011.
--
-- 3 June 2011  Changed from a test module to being the base
--              module of the Expr class.
--
-- Copyright (c) 2011, Antiope Associates, all rights reserved.
--

module Math.Symbolic.Wheeler.Expr (
       Expr (..),
       function,
       repSpaces,
       hasNonCommuting,
       showInternal,
       negate, (+), (-), (*), (/),
       mapExpr
) where


import Data.List
import Data.Maybe
import Data.Ratio
import System.IO.Unsafe


import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Canonicalize
--import Math.Symbolic.Wheeler.CanonicalizeDebug
import Math.Symbolic.Wheeler.Complexity
import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Function
import Math.Symbolic.Wheeler.IO
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.Numeric
import Math.Symbolic.Wheeler.Precedence
import Math.Symbolic.Wheeler.SumOrd
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.SimpleSymbol
import Math.Symbolic.Wheeler.UniqueID



-- The Expr data type:
--
data Expr where
    Const     :: Numeric -> Expr
    Applic    :: Function -> Expr -> Expr
    Symbol    :: Symbol -> Expr
    Sum       :: [ Expr ] -> Expr
    Product   :: [ Expr ] -> Expr
    Power     :: Expr -> Expr -> Expr
    Undefined :: Expr


--
-- Show instance for an expression.
--
-- The show instance is complicated by three things, 1) parenthesization,
-- 2) leading factors of -1 and 3) negative integer powers.
--
-- 

instance Show Expr where
    showsPrec d (Const x)      = showParen (d > applicationPrecedence) $ showsPrec d x
    showsPrec d (Applic f arg) = showParen (d > applicationPrecedence) $ showsPrec d f  .
                                                                         showString " " .
                                                                         showsPrec applicationPrecedence arg
    showsPrec d (Symbol s)     = showsPrec d s

    showsPrec _ (Sum [])       = showString ""
    showsPrec d (Sum (t : ts)) = showParen (d > sumPrecedence) $ showFirstTerm t .
                                                                 showTerms ts
        where
            showFirstTerm :: Expr -> ShowS
            showFirstTerm p@(Product (Const (I n) : xs))
                | n == -1   = showString "- " .
                              showFactor xs
                | n  < -1   = showIntegerFactor n .
                              showFactor xs
                | otherwise = showsPrec sumPrecedence p
            showFirstTerm p@(Product (Const (Q num denom) : xs))
                | num <= -1 = showRationalFactor num denom .
                              showFactor xs
                | otherwise = showsPrec sumPrecedence p
            showFirstTerm x = showsPrec sumPrecedence x

            showTerms :: [ Expr ] -> ShowS
            showTerms [] = showString ""
            showTerms (p@(Product (Const (I n) : xs)) : xss)
                | n == -1   = showString " - " .
                              showFactor xs    .
                              showTerms xss
                | n  < -1   = showIntegerFactor n .
                              showFactor xs       .
                              showTerms xss
                | otherwise = showString " + "          .
                              showsPrec sumPrecedence p .
                              showTerms xss
            showTerms (p@ (Product (Const (Q num denom) : xs)) : xss)
                | num <= -1 = showRationalFactor num denom .
                              showFactor xs                .
                              showTerms xss
                | otherwise = showString " + "          .
                              showsPrec sumPrecedence p .
                              showTerms xss

            showTerms (x : []) = showTerm x
            showTerms (x : xs) = showTerm x . showTerms xs

            showTerm :: Expr -> ShowS
            showTerm x = showString " + " . showsPrec sumPrecedence x

            showIntegerFactor n = showString " - "  .
                                  shows (-n)        .
                                  showString " * "
            showRationalFactor num denom = showString " - (" .
                                           shows (-num)        .
                                           showString "/"    .
                                           shows denom           .
                                           showString ") * "
            showFactor []       = showString ""
            showFactor (x : []) = showsPrec sumPrecedence x
            showFactor xs       = showsPrec sumPrecedence (Product xs)


    showsPrec d (Product f) =
        let
            (pos, neg) = collectPowers f

            -- Note that we define non-commuting factors
            -- to always be positive, since this avoids
            -- ordering ambiguities.
            --
            collectPowers :: [ Expr ] -> ([ Expr ], [ Expr ])
            collectPowers es = partition (not . negPower) es
                where
                    negPower (Power b (Const (I n)))
                        | n < 0 && not (hasNonCommuting b) = True
                        | otherwise                        = False
                    negPower (Power b (Const (Q n _)))
                        | n < 0 && not (hasNonCommuting b) = True
                        | otherwise = False
                    negPower _ = False
                 
            recipPower :: Expr -> Expr
            recipPower (Power b (Const (I n)))
                | n == -1   =  b
                | otherwise = Power b (Const (negate (I n)))
            recipPower (Power b (Const (Q num denom))) = Power b (Const (negate (Q num denom)))
            recipPower _ = error "only know how to show reciprocal powers of integers and rationals"

            showFactors :: ([ Expr ], [ Expr ]) -> ShowS
            showFactors ([], (y : [])) = showString "1 / " .
                                         showsPrec productPrecedence (recipPower y)
            showFactors ([], ys)       = showString "1 / ("              .
                                         showFactors' (map recipPower ys) .
                                         showString ")"
            showFactors (x, [])        = showFactors' x
            showFactors (x, (y : []))  = showFactors' x .
                                         showString " / "        .
                                         showsPrec productPrecedence (recipPower y)
            showFactors (x, ys)        = showFactors' x                  .
                                         showString " / ("               .
                                         showFactors' (map recipPower ys) .
                                         showString ")"

            showFactors' :: [ Expr ] -> ShowS
            showFactors' [] = showString ""
            showFactors' (c@(Const (I n)) : [])
                 | n == -1   = showString "-"
                 | otherwise = showsPrec productPrecedence c
            showFactors' (x : []) = showsPrec productPrecedence x
            showFactors' (c@(Const (I n)) : xs)
                 | n == -1   = showString "-" .
                               showFactors' xs
                 | otherwise = showsPrec productPrecedence c .
                               showString " * "              .
                               showFactors' xs 
            showFactors' (x : xs) = showsPrec productPrecedence x .
                                    showString " * "              .
                                    showFactors' xs
        in
            showParen (d > productPrecedence) $ showFactors (pos, neg)

    showsPrec d p@(Power _ _) = showParen (d > applicationPrecedence) $ showPower p
        where
            showPower :: Expr -> ShowS
            showPower p'@(Power b (Const (I n)))
                | hasNonCommuting b && n > 0 = showPower'  p'
                | hasNonCommuting b && n < 0 = showPower'' p'
                | n == -1                    = showRecip b
                | n  <  0                    = showRecipPower (Power b (Const (negate (I n))))
                | otherwise                  = showPower' p'
            showPower p'@(Power b (Const (Q num denom)))
                | hasNonCommuting b = showPower'' p'
                | num < 0           = showRecipPower (Power b (Const (negate (Q num denom))))
                | otherwise         = showPower' p'
            showPower p' = showPower' p'

            showPower'  (Power b e) = showsPrec powerPrecedence b .
                                      showString "**"             .
                                      showsPrec powerPrecedence e
            showPower' _            = error "Applying showPower' to something not a Power"

            showPower'' (Power b e) = showsPrec powerPrecedence b .
                                      showString "**"             .
                                      showString "("              .
                                      showsPrec powerPrecedence e .
                                      showString ")"
            showPower'' _           = error "Applying showPower'' to something not a Power"

            showRecipPower pow = showString "1 / " . showPower' pow

            showRecip e = showString "1 / ". showsPrec productPrecedence e


    showsPrec _ Undefined     = showString "Undefined"



-- showInternal displays the internal form of an expression.  It is equivalent
-- to what you would see if Expr and the types on which it depends on all had
-- automatically derived instances of Show.

-- Again, understand the parenthesization and fix up the display of integers
-- and rationals.

showInternal :: Expr -> String
showInternal e = showsExpr_ 0 e $ ""

showsExpr_ :: Int -> Expr -> ShowS
showsExpr_ d (Const x)         = showParen (d > listPrecedence) $ showString "Const " .
                                                                  showsPrec (applicationPrecedence + 1) x
showsExpr_ d (Applic f arg)    = showParen (d > listPrecedence) $ showsApplic listPrecedence f .
                                                                  showString " "               .
                                                                  showsExpr_ (applicationPrecedence + 1) arg
showsExpr_ d (Symbol s)        = showParen (d > listPrecedence) $ showString "Symbol " .
                                                                  showsSymbol_ s
showsExpr_ d (Sum terms)       = showParen (d > listPrecedence) $ showString "Sum " .
                                                                  showsList_ (showsExpr_ listPrecedence) terms
showsExpr_ d (Product factors) = showParen (d > listPrecedence) $ showString "Product " .
                                                                  showsList_ (showsExpr_ listPrecedence) factors
showsExpr_ d (Power b e)       = showParen (d > listPrecedence) $ showString "Power " .
                                                                  showsExpr_ nonListPrecedence b .
                                                                  showString " "                 .
                                                                  showsExpr_ nonListPrecedence e
showsExpr_ d Undefined         = showParen (d > listPrecedence) $ showString "Undefined"


showsApplic :: Int -> Function -> ShowS
showsApplic d f = showParen (d > listPrecedence) $ showString "Applic " .
                                                   showsPrec nonListPrecedence f

showsList_ :: (a -> ShowS) -> [a] -> ShowS
showsList_ _     []     s = "[]" ++ s
showsList_ showx (x:xs) s = '[' : showx x (showl xs)
    where
        showl []     = ']' : s
        showl (y:ys) = ',' : showx y (showl ys)


instance Eq (Expr) where
    (==) (Const x) (Const y)         = x  == y
    (==) (Symbol x) (Symbol y)       = x  == y
    (==) (Power b e) (Power b' e')   = b  == b' && e == e'
    (==) (Sum ts) (Sum ts')          = ts == ts'
    (==) (Product fs) (Product fs')  = fs == fs'
    (==) (Applic f a) (Applic f' a') = f  == f' && a == a'
    (==) _            _              = False


-- Expressions are instances of the Ord class.  This allows
-- putting them in a canonical form.
--
-- The entries for this instance are taken from the description
-- in J. S. Cohen, "Computer Algebra and Symbolic Computation:
-- Mathematical Methods", chapter 3.  In particular, the definitions
-- below follow the description in Figure 3.9. 

instance Ord (Expr) where
    compare (Const x) (Const y)             = compare x y
    compare (Const _) _                     = LT
         
    compare (Product _) (Const _)           = GT
    compare (Product x) (Product y)         = compareList x y
    compare p@(Product _) y                 = compare p (Product [ y ])

    compare (Power _ _) (Const _)           = GT
    compare p@(Power _ _) (Product y)       = compareList [ p ] y
    compare p@(Power _ _) p'@(Power _ _)    = comparePower p p'
    compare p@(Power _ _) y                 = comparePower p (Power y (Const 1))

    compare (Sum _) (Const _)               = GT
    compare s@(Sum _) p@(Product _)         = compare (Product [ s ]) p
    compare s@(Sum _) p@(Power _ _)         = compare (Power s (Const 1)) p
    compare (Sum x) (Sum y)                 = compareList x y
    compare s@(Sum _) y                     = compare s (Sum [ y ])

    compare (Applic _ _) (Const _)          = GT
    compare a@(Applic _ _) p@(Product _)    = compare (Product [ a ]) p
    compare a@(Applic _ _) p@(Power _ _)    = compare (Power a (Const 1)) p
    compare a@(Applic _ _) s@(Sum _)        = compare (Sum [ a ]) s
    compare a@(Applic _ _) a'@(Applic _ _)  = compareApplication a a'
    compare (Applic (Func f) _) (Symbol y)  = compare f (name y)
    compare (Applic _ _) (Symbol _)         = GT 

    compare (Symbol _) (Const _)            = GT
    compare s@(Symbol _) p@(Product _)      = compare (Product [ s ]) p
    compare s@(Symbol _) p@(Power _ _)      = compare (Power s (Const (I 1))) p
    compare s@(Symbol _) s'@(Sum _)         = compare (Sum [ s ]) s'
    compare (Symbol x) (Applic (Func f) _)  = compare (name x) f
    compare (Symbol _) (Applic _ _)         = LT 
    compare (Symbol x) (Symbol y)           = compare x y

    compare _ Undefined                     = GT
    compare Undefined _                     = GT



-- The compareList function needs to take into account
-- noncommutativity!
--
-- CHECK ME! Should the expressions in the list be reversed?
--
compareList :: [ Expr ] -> [ Expr ] -> Ordering
compareList [] [] = EQ
compareList _  [] = GT
compareList [] _  = LT
compareList x  y  =
    if mutuallyNonCommuting x y
        then GT
        else
            let
                (x' : xs') = reverse x
                (y' : ys') = reverse y

                compareList' :: [ Expr ] -> [ Expr ] -> Ordering
                compareList' [] [] = EQ
                compareList' _  [] = GT
                compareList' [] _  = LT
                compareList' (a : as) (b : bs) =
                    if a /= b then compare a b else compareList' as bs
            in
                if x' /= y' then compare x' y' else compareList' xs' ys'


comparePower :: Expr
             -> Expr
             -> Ordering
comparePower (Power b e) (Power b' e')
    | b /= b'   = compare b b'
    | otherwise = compare e e'
comparePower _ _ = error "comparePower arguments not both Powers."


compareApplication :: Expr
                   -> Expr
                   -> Ordering
compareApplication (Applic f x) (Applic g y) =
    if mutuallyNonCommuting [x] [y]
        then GT
        else if f /= g
             then compare f g
             else compare x y
compareApplication _ _ = error "compareApplication arguments not both Applics."



-- Expressions are also instances of the "SumOrd" class.
-- This is class defines the "sumCompare" operation, which
-- is only used to compare terms within a Sum.  In almost
-- every case it reduces to the usual comparison operation
-- defined above.  However, when comparing Symbols it is different,
-- since Symbols with a noncommuting Product may be reordered within
-- a Sum.  This let us canonicalize sums of noncommuting symbols.

instance SumOrd Expr where
    sumCompare (Const x) (Const y)             = compare x y
    sumCompare (Const _) _                     = LT
         
    sumCompare (Product _) (Const _)           = GT
    sumCompare (Product x) (Product y)         = sumCompareList x y
    sumCompare p@(Product _) y                 = sumCompare p (Product [ y ])

    sumCompare (Power _ _) (Const _)           = GT
    sumCompare p@(Power _ _) (Product y)       = sumCompareList [ p ] y
    sumCompare p@(Power _ _) p'@(Power _ _)    = sumComparePower p p'
    sumCompare p@(Power _ _) y                 = sumComparePower p (Power y (Const 1))

    sumCompare (Sum _) (Const _)               = GT
    sumCompare s@(Sum _) p@(Product _)         = sumCompare (Product [ s ]) p
    sumCompare s@(Sum _) p@(Power _ _)         = sumCompare (Power s (Const 1)) p
    sumCompare (Sum x) (Sum y)                 = sumCompareList x y
    sumCompare s@(Sum _) y                     = sumCompare s (Sum [ y ])

    sumCompare (Applic _ _) (Const _)          = GT
    sumCompare a@(Applic _ _) p@(Product _)    = sumCompare (Product [ a ]) p
    sumCompare a@(Applic _ _) p@(Power _ _)    = sumCompare (Power a (Const 1)) p
    sumCompare a@(Applic _ _) s@(Sum _)        = sumCompare (Sum [ a ]) s
    sumCompare a@(Applic _ _) a'@(Applic _ _)  = sumCompareApplication a a'
    sumCompare (Applic (Func f) _) (Symbol y)  = compare f (name y)
    sumCompare (Applic _ _) (Symbol _)         = GT 

    sumCompare (Symbol _) (Const _)            = GT
    sumCompare s@(Symbol _) p@(Product _)      = sumCompare (Product [ s ]) p
    sumCompare s@(Symbol _) p@(Power _ _)      = sumCompare (Power s (Const (I 1))) p
    sumCompare s@(Symbol _) s'@(Sum _)         = sumCompare (Sum [ s ]) s'
    sumCompare (Symbol x) (Applic (Func f) _)  = compare (name x) f
    sumCompare (Symbol _) (Applic _ _)         = LT 
    sumCompare (Symbol x) (Symbol y)           = sumCompare x y

    sumCompare _ Undefined                     = GT
    sumCompare Undefined _                     = GT


-- The compareSumList function neglects whether the terms commute
-- or not:
-- This function is wrong!!!  reverses multiple times on the recursive
-- call! XXX FIXME XXX

sumCompareList :: [ Expr ]
               -> [ Expr ]
               -> Ordering
sumCompareList [] [] = EQ
sumCompareList _  [] = GT
sumCompareList [] _  = LT
sumCompareList x  y  =
    let
        (x' : xs') = reverse x
        (y' : ys') = reverse y

        sumCompareList' :: [ Expr ] -> [ Expr ] -> Ordering
        sumCompareList' [] [] = EQ
        sumCompareList' _  [] = GT
        sumCompareList' [] _  = LT
        sumCompareList' (a : as) (b : bs) =
           if a /= b then sumCompare a b else sumCompareList' as bs
    in
        if x' /= y' then sumCompare x' y' else sumCompareList' xs' ys'


sumComparePower :: Expr
                -> Expr
                -> Ordering
sumComparePower (Power b e) (Power b' e')
    | b /= b'   = sumCompare b b'
    | otherwise = sumCompare e e'
sumComparePower _ _ = error "sumComparePower arguments not both Powers."


sumCompareApplication :: Expr
                      -> Expr
                      -> Ordering
sumCompareApplication (Applic f x) (Applic g y)
    | f /= g    = compare f g
    | otherwise = sumCompare x y
sumCompareApplication _ _ = error "sumCompareApplication arguments not both Applics."


-- Check if any of the expressions in the argument lists
-- fail to commute.  If that is the case, return True.

mutuallyNonCommuting :: [ Expr ] -> [ Expr ] -> Bool
mutuallyNonCommuting x y =
    if any hasNonCommuting x && any hasNonCommuting y
        then (not . null) (concatMap repSpaces x `intersect` concatMap repSpaces y)
        else False


hasNonCommuting :: Expr -> Bool
hasNonCommuting Undefined      = False
hasNonCommuting (Const _)      = False
hasNonCommuting (Symbol s)     = isNonCommuting s
hasNonCommuting (Sum ts)       = any hasNonCommuting ts
hasNonCommuting (Product fs)   = any hasNonCommuting fs
hasNonCommuting (Power b e)    = hasNonCommuting b || hasNonCommuting e
hasNonCommuting (Applic _ arg) = hasNonCommuting arg


repSpaces :: Expr -> [ String ]
repSpaces e = sort $ nub $ rps [] e
    where
        rps acc Undefined      = acc
        rps acc (Const _)      = acc
        rps acc (Symbol s)     = if isJust (repSpace s) then (fromJust (repSpace s)) : acc else acc
        rps acc (Sum ts)       = concatMap (\e' -> rps acc e') ts
        rps acc (Product fs)   = concatMap (\e' -> rps acc e') fs
        rps acc (Power b ex)   = rps [] b ++ rps acc ex
        rps acc (Applic _ arg) = rps acc arg


function :: Expr -> Maybe Function
function (Applic f _) = Just f

function _            = Nothing


instance Num Expr where
         (+) f g       = canonicalize (Sum [f, g])
         (-) f g       = canonicalize (Sum [f, negate g])
         (*) f g       = canonicalize (Product [f, g])
         negate f      = canonicalize (Product [Const (-1), f])
         abs f         = canonicalize (Applic Abs f)
         signum f      = canonicalize (Applic Signum f)
         fromInteger n = Const (I n)
         

instance Fractional Expr where
         fromRational r                  = Const (Q (numerator r) (denominator r))
         (/) (Const (I n)) (Const (I d)) = Const (Q n d)
         (/) n d                         = canonicalize (Product [n, Power d (Const (-1))])
         recip r                         = canonicalize (1 / r)


instance Floating Expr where
         pi       = Symbol $ unsafePerformIO $ do
                        ident <- nextId
                        return $ Simple S { simpleIdentifier    = ident
                                          , simpleName          = "pi"
                                          , simpleTeXName       = "\\pi"
                                          , simpleType          = Regular
                                          , simpleComplexity    = Real
                                          , simpleCommutativity = Commuting
                                          }
         exp x    = Applic Exp x
         log x    = Applic Log x
         sin x    = Applic Sin x
         cos x    = Applic Cos x
         tan x    = Applic Tan x
         asin x   = Applic Asin x
         acos x   = Applic Acos x
         atan x   = Applic Atan x
         sinh x   = Applic Sinh x
         cosh x   = Applic Cosh x
         tanh x   = Applic Tanh x
         asinh x  = Applic Asinh x
         acosh x  = Applic Acosh x
         atanh x  = Applic Atanh x
         sqrt x   = Power x (Const (Q 1 2))
         (**) b e = canonicalize (Power b e)

-- An Enum instance. Note that "fromEnum" only
-- makes sense for constants.

instance Enum Expr where
    succ x                   = x + 1
    pred x                   = x - 1
    toEnum                   = fromIntegral
    fromEnum (Const (I n))   = fromIntegral n
    fromEnum (Const (Q n d)) = fromIntegral (n `quot` d) 
    fromEnum _               = error "fromEnum applied to non-enumerable expression"


instance Read Expr where
    readsPrec _ s = [ (readExpr s, "") ]



mapExpr :: (Expr -> Expr) -> Expr -> Expr
mapExpr f (Sum ts)     = f (Sum (map (mapExpr f) ts))
mapExpr f (Product fs) = f (Product (map (mapExpr f) fs))
mapExpr f e = f e

