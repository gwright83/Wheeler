{-# OPTIONS_GHC -XFlexibleInstances #-}
--
-- Matcher.hs
--
-- Match a pattern expression against another expression;
-- return the matching subexpression.
--
-- Gregory Wright, 15 June 2012
--

module Math.Symbolic.Wheeler.Matcher where


import Data.List
--import qualified Data.Map as Map
import Data.Maybe


import Math.Symbolic.Wheeler.Canonicalize
import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Expr
--import Math.Symbolic.Wheeler.Symbol
--import Math.Symbolic.Wheeler.Tensor


-- The data structure for patterns is a Rose Tree.  In a
-- patterm, the data item at each node is a predicate.
--
data Rose a = Rose a [ Rose a ] deriving Show

type Pred = Expr -> Bool
type Pattern = Rose Pred


-- Some basic predicates:

isSum :: Pred
isSum (Sum _) = True
isSum _       = False

isProduct :: Pred
isProduct (Product _) = True
isProduct _           = False

isLeafExpr :: Expr -> Pred
isLeafExpr ex = (==) ex


-- compile turns an Expr into a Pattern 
--
compile :: Expr -> Pattern
compile = compile' . canonicalize

compile' :: Expr -> Pattern
compile' (Sum ts)       = Rose isSum (map compile ts)
compile' (Product fs)   = Rose isProduct (map compile fs)
compile' ex             = Rose (isLeafExpr ex) []


-- The match function checks if the pattern expression is contained
-- anywhere in the subject expression.
--
match :: Pattern -> Expr -> Bool
match pat s@(Sum ts)     = oneMatch pat s || any (match pat) ts
match pat p@(Product fs) = oneMatch pat p || any (match pat) fs
match pat ex             = oneMatch pat ex


-- the oneMatch function checks if the pattern expression
-- is contained in the a subtree beginning at the root of
-- the subject expression.
--
oneMatch :: Pattern -> Expr -> Bool
oneMatch (Rose p ps) s@(Sum ts)     = p s && unorderedMatch ps ts
oneMatch (Rose p ps) f@(Product fs) = p f && productMatch   ps fs
oneMatch (Rose p _)  ex             = p ex


productMatch :: [ Pattern ] -> [ Expr ] -> Bool
productMatch ps xs =
  let
    factorsByRepSpace   = groupFactors xs
    commutingFactors    = lookup [] factorsByRepSpace
    noncommutingFactors = concat $ map snd $ filter (not . null . fst) factorsByRepSpace
    
    leftoverPatterns    = if isJust commutingFactors
                          then notMatching ps (fromJust commutingFactors)
                          else ps
  in
    infixMatch leftoverPatterns noncommutingFactors

  
-- notMatching matches each pattern on the list, returning
-- the patterns that did not match.
--
notMatching :: [ Pattern ] -> [ Expr ] -> [ Pattern ]
notMatching [] _  = []
notMatching (p : ps) xs = if any (oneMatch p) xs
                          then notMatching ps xs
                          else p : notMatching ps xs


-- unorderedMatch asks if each element of the list of the predicate p
-- partially applied to the pattern list, returns True.  As each
-- predicate is applied, the matching element is deleted, so if the
-- pattern contains two instances of an expression, two corresponding
-- instances must be present in the subject list.
--
unorderedMatch :: [ Pattern ] -> [ Expr ] -> Bool
unorderedMatch [] _       = True
unorderedMatch _ []       = False
unorderedMatch (p : ps) y =
  let
    p' = oneMatch p
  in
    any p' y && unorderedMatch ps (deleteAt p' y) 


-- orderedMatch is similar to unorderedMatch, but the list
-- of partially applied predicates must match in order.  This
-- means that (p x_1) must match earlier in the subject list
-- than (p x_2).
--
orderedMatch :: [ Pattern ] -> [ Expr ] -> Bool
orderedMatch [] _ = True
orderedMatch _ [] = False
orderedMatch (p : ps) y =
  let
    p' = oneMatch p
  in
    any p' y && orderedMatch ps (deleteUpTo p' y)


-- prefixMatch tests if the patterns match the leading elements
-- of the list of subject expression.
--
prefixMatch :: [ Pattern ] -> [ Expr ] -> Bool  
prefixMatch [] _ = True
prefixMatch _ [] = False
prefixMatch (p : ps) (x : xs) =
  let
    p' = oneMatch p 
  in
    p' x && prefixMatch ps xs


-- infixMatch tests if the patterns match contiguous
-- elements of the list of subject expressions.
--
infixMatch :: [ Pattern ] -> [ Expr ] -> Bool
infixMatch needle haystack = any (prefixMatch needle) (tails haystack)


-- deleteAt is the useful but mysteriously unavailable
-- function that deletes the first element matching a
-- predicate, returning a list without that element.
--
deleteAt :: (a -> Bool) -> [ a ] -> [ a ]
deleteAt _ []       = []
deleteAt p (x : xs) = if p x then xs else x : deleteAt p xs


-- deleteUpTo is a variant of the above, which deletes
-- elements of a list until the predicate returns True,
-- returning the list that follows the matching element.
--
deleteUpTo :: (a -> Bool) -> [ a ] -> [ a ]
deleteUpTo _ []       = []
deleteUpTo p (x : xs) = if p x then xs else deleteUpTo p xs


matchAll :: Pattern -> Expr -> [ Breadcrumbs ]
matchAll pat ex = matchAll' [] [] pat ex
    where
      matchAll' :: Breadcrumbs
                -> [ Breadcrumbs ]
                -> Pattern
                -> Expr
                -> [ Breadcrumbs ]
      matchAll' bc bcs pt s@(Sum ts)     = if oneMatch pt s
                                               then (bc : bcs)
                                               else foldr (\(n, x) b -> matchAll' (Scxt n : bc) b pt x) bcs (zip [1..] ts)
      matchAll' bc bcs pt p@(Product fs) = if oneMatch pt p
                                               then (bc : bcs)
                                               else foldr (\(n, x) b -> matchAll' (Pcxt n : bc) b pat x) bcs (zip [1..] fs)
      matchAll' bc bcs pt e              = if oneMatch pt e then bc : bcs else bcs


-- type Environ = Map.Map String (VarIndex, Breadcrumbs)

-- matchAll_ :: Pattern -> Expr -> [ (Breadcrumbs, Environ) ]
-- matchAll_ pat ex = matchAll_' ([], Map.empty) [] pat ex
--     where
--       matchAll_' :: (Breadcrumbs, Environ)      -- breadcrumb trail for the current position
--                  -> [ (Breadcrumbs, Environ) ]  -- Accumulated breadcrumb trails
--                  -> Pattern                     -- Patern for which we are searching
--                  -> Expr                        -- Subject expression
--                  -> [ (Breadcrumbs, Environ) ]  -- Returned list of breadcrumb trails.
--       matchAll_' (bc, env) bcs pt s@(Sum ts) =
--         let
--           (mat, env') = oneMatch_' env pt s
--         in
--          if mat
--          then (bc, env') : bcs
--          else foldr (\(n, x) b -> matchAll_' (Scxt n : bc, env) b pt x) bcs (zip [1..] ts)
              
--       matchAll_' (bc, env) bcs pt p@(Product fs) =
--         let
--           (mat, env') = oneMatch_' env pt p
--         in
--          if mat
--          then (bc, env') : bcs
--          else foldr (\(n, x) b -> matchAll_' (Pcxt n : bc, env) b pat x) bcs (zip [1..] fs)
              
--       matchAll_' (bc, env) bcs pt e =
--         let
--           (mat, env') = oneMatch_' env pt e
--         in
--          if mat
--          then (bc, env') : bcs
--          else bcs


-- oneMatch_' :: Environ -> Pattern -> Expr -> (Bool, Environ)
-- oneMatch_' env (Rose p ps) s@(Sum ts) =
--   let
--     (mat, env') = unorderedMatch_ env ps ts 
--   in (p s && mat, env')
-- oneMatch_' env (Rose p ps) f@(Product fs) = 
--   let
--     (mat, env') = productMatch_ env ps fs
--   in (p f && mat, env')
-- oneMatch_' env (Rose p _)  t@(Symbol (Tensor _)) = (p t,  env)
-- oneMatch_' env (Rose p _)  ex                    = (p ex, env)


-- unorderedMatch_ :: Environ -> [ Pattern ] -> [ Expr ] -> (Bool, Environ)
-- unorderedMatch_ _ [] _       = True
-- unorderedMatch_ _ _  []      = False
-- unorderedMatch_ env (p : ps) y =
--   let
--     p' = oneMatch_' env p
--   in
--     any p' y && unorderedMatch_ env ps (deleteAt (fst . p') y) 


-- productMatch_ :: Environ -> [ Pattern ] -> [ Expr ] -> Bool
-- productMatch_ env ps xs =
--   let
--     factorsByRepSpace   = groupFactors xs
--     commutingFactors    = lookup [] factorsByRepSpace
--     noncommutingFactors = concat $ map snd $ filter (not . null . fst) factorsByRepSpace
    
--     leftoverPatterns    = if isJust commutingFactors
--                           then notMatching ps (fromJust commutingFactors)
--                           else ps
--   in
--     infixMatch leftoverPatterns noncommutingFactors



-- Try a different approach: make a list of subtrees, then test each
-- one to see if it matches.
--
-- A nice aspect of this approach is that it's easy to add position
-- information to the list.
--

subExprs :: Expr -> [ Expr ]
subExprs s@(Sum ts)     = s : concatMap subExprs ts
subExprs p@(Product fs) = p : concatMap subExprs fs
subExprs e              = e : []


subExprs' :: Expr -> [ (Breadcrumbs, Expr) ]
subExprs' ex = subEx [] ex
  where
    subEx :: Breadcrumbs -> Expr -> [ (Breadcrumbs, Expr) ]
    subEx bc s@(Sum ts)     = (bc, s) : concatMap (\(n, x) -> subEx (Scxt n : bc) x) (zip [1..] ts)    
    subEx bc p@(Product fs) = (bc, p) : concatMap (\(n, x) -> subEx (Pcxt n : bc) x) (zip [1..] fs)
    subEx bc e              = (bc, e) : []
    

neoMatch :: Pattern -> Expr -> [ (Breadcrumbs, Expr) ]
neoMatch p e = filter ((oneMatch p) . snd) (subExprs' e)
