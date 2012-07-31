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


import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Data.Maybe


import Math.Symbolic.Wheeler.Canonicalize
import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.DiracSpinor
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.SimpleSymbol
import Math.Symbolic.Wheeler.Tensor


-- Pattern variables: move to a separate module
-- eventually.

data PatternVar = SimplePattern S
                | TensorPattern T
                | SpinorPattern D
                | VarIndexPattern VarIndex
                deriving (Eq, Ord, Show)


-- The data structure for patterns is a Rose Tree.  In a
-- patterm, the data item at each node is a predicate.
--
data Rose a = Rose a [ Rose a ] deriving Show

type Environ = Map.Map PatternVar Expr
--type Environ = Map.Map PatternVar (PatternVar, Breadcrumbs)

type Pred    = Expr -> State Environ Bool
type Pattern = Rose Pred


-- Some basic predicates:

isSum :: Pred
isSum (Sum _) = return True
isSum _       = return False

isProduct :: Pred
isProduct (Product _) = return True
isProduct _           = return False

isLeafExpr :: Expr -> Pred
isLeafExpr ex@(Symbol (Simple _)) = \e -> do
  env <- get
  let
    mat = ex == e
  
  if mat && hasPattern ex
    then do
        let (mat', env') = checkEnvironAndUpdate env ex e
        put env'
        return mat'
    else return mat
isLeafExpr ex@(Symbol (Tensor _)) = \e -> do
  env <- get
  let
    mat = ex == e
  
  if mat && hasPattern indexPatterns _ = []

    then do
        let (mat', env') = checkEnvironAndUpdate env ex e
        put env'
        return mat'
    else return mat
isLeafExpr ex = \e -> do return ((==) ex e)


hasPattern :: Expr -> Bool
hasPattern (Symbol (Simple s)) = simpleType s == PatternSimple
hasPattern _ = False


getPatterns :: Expr -> [ PatternVar ]
getPatterns (Symbol (Simple s)) = if simpleType s == PatternSimple
                                 then [SimplePattern s]
                                 else []
getPatterns (Symbol (Tensor t)) = if tensorType t == PatternTensor
                                 then [TensorPattern t] ++ indexPatterns t
                                 else indexPatterns t
getPatterns _ = []


indexPatterns :: T -> [ PatternVar ]
indexPatterns t = let
  indices  = slots t
  indices' = filter (\i -> (==) PatternIndex (varIndexType i)) indices
  
  varIndexType (Covariant (Abstract i))      = indexType i
  varIndexType (Contravariant (Abstract i))  = indexType i
  varIndexType (Covariant (Component _))     = RegularIndex
  varIndexType (Contravariant (Component _)) = RegularIndex
  in
   map VarIndexPattern indices'


-- Need to modify this so it processes a whole list
-- of patterns.  So, should EVERY pattern match or
-- ANY pattern match?
--
checkEnvironAndUpdate :: Environ -> Expr -> Expr -> (Bool, Environ)
checkEnvironAndUpdate env pat_ex ex =
  let
    (p : _) = getPatterns pat_ex
    ex' = Map.lookup p env
  in
   if isJust ex'
   then ((fromJust ex') == ex, env)
   else (True, Map.insert p ex env)


-- Try a different approach: make a list of subtrees, then test each
-- one to see if it matches.
--
-- A nice aspect of this approach is that it's easy to add position
-- information to the list.
--
match :: Pattern -> Expr -> [ (Breadcrumbs, Expr, Environ) ]
match p e = map snd . filter fst $ map (oneMatch p) (subExprs e)


-- compile turns an Expr into a Pattern 
--
compile :: Expr -> Pattern
compile = compile_ . canonicalize

compile_ :: Expr -> Pattern
compile_ (Sum ts)       = Rose isSum (map compile ts)
compile_ (Product fs)   = Rose isProduct (map compile fs)
compile_ ex             = Rose (isLeafExpr ex) []


-- Given an expression, generate a list of subtrees and the breadcrumb
-- trail to the root of each subtree.
--
subExprs :: Expr -> [ (Breadcrumbs, Expr) ]
subExprs ex = subEx [] ex
  where
    subEx :: Breadcrumbs -> Expr -> [ (Breadcrumbs, Expr) ]
    subEx bc s@(Sum ts)     = (bc, s) : concatMap (\(n, x) -> subEx (Scxt n : bc) x) (zip [1..] ts)    
    subEx bc p@(Product fs) = (bc, p) : concatMap (\(n, x) -> subEx (Pcxt n : bc) x) (zip [1..] fs)
    subEx bc e              = (bc, e) : []
    

-- Given a pattern and a single expression, test if it
-- matches.  The expression is assumed to be a subtree of
-- a given expression, so it comes with a breadcrumb trail.
--
-- An environment containing the matches for pattern variables
-- is also returned.
--
oneMatch :: Pattern -> (Breadcrumbs, Expr) -> (Bool, (Breadcrumbs, Expr, Environ))
oneMatch p (bc, e) = let
  (mat, env) = runState (oneMatch_ p e) Map.empty
  in
   (mat, (bc, e, env))
   

-- Match a single pattern against an expression.  The State
-- monad contains the Environ of pattern variables and the
-- corresponding matched variables.
--
oneMatch_ :: Pattern -> Expr -> State Environ Bool
oneMatch_ (Rose p ps) s@(Sum ts)     = do
  v  <- p s
  v' <- unorderedMatch ps ts
  return (v && v')
oneMatch_ (Rose p ps) f@(Product fs) = do
  v  <- p f
  v' <- productMatch   ps fs
  return (v && v')
oneMatch_ (Rose p _)  ex             = p ex


-- Match the patterns (in any order) against the members of
-- the expression list.
--
unorderedMatch :: [ Pattern ] -> [ Expr ] -> State Environ Bool
unorderedMatch [] _       = do return True
unorderedMatch _ []       = do return False
unorderedMatch (p : ps) y = do
  let
    p' = \e -> evalState (oneMatch_ p e) Map.empty
  v <- unorderedMatch ps (deleteAt p' y)  
  return $
    any p' y && v 


-- Match the patterns against the members of the expression list
-- in a way appropriate to a product: in any order for commuting
-- factors, in order for noncommuting factors in the same
-- representation space.
--
productMatch :: [ Pattern ] -> [ Expr ] -> State Environ Bool
productMatch ps xs = do
  let
    factorsByRepSpace   = groupFactors xs
    commutingFactors    = lookup [] factorsByRepSpace
    noncommutingFactors = concat $ map snd $ filter (not . null . fst) factorsByRepSpace
    
    leftoverPatterns    = if isJust commutingFactors
                          then notMatching ps (fromJust commutingFactors)
                          else ps
  
  infixMatch leftoverPatterns noncommutingFactors


-- Return a list of patterns that are not satisfied by any
-- element of the expression list.
--
notMatching :: [ Pattern ] -> [ Expr ] -> [ Pattern ]
notMatching [] _  = []
notMatching (p : ps) xs = if any (\e -> evalState (oneMatch_ p e) Map.empty) xs
                          then notMatching ps xs
                          else p : notMatching ps xs


-- Test if the patterns are satisfied (in order) by the leading
-- elements of the expression list.
--
prefixMatch :: [ Pattern ] -> [ Expr ] -> State Environ Bool  
prefixMatch [] _ = do return True
prefixMatch _ [] = do return False
prefixMatch (p : ps) (x : xs) = do
  v <- prefixMatch ps xs
  let p' = \e -> evalState (oneMatch_ p e) Map.empty
  return (p' x && v)


-- Test if the patterns are satisfied by contiguous members
-- of the expression list.
--
infixMatch :: [ Pattern ] -> [ Expr ] -> State Environ Bool
infixMatch needle haystack = do
  let p' = \es -> evalState (prefixMatch needle es) Map.empty
  return (any p' (tails haystack))


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

