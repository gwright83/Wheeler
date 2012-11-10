--
-- Replacer.hs
--
-- Replace a subexpression with a different one.
--
-- Gregory Wright, 18 September 2012
--


module Math.Symbolic.Wheeler.Replacer where


import qualified Data.DList as DList
import qualified Data.Map as Map
import Data.Maybe

import Math.Symbolic.Wheeler.Canonicalize
import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Matchable
import Math.Symbolic.Wheeler.Matcher2
import Math.Symbolic.Wheeler.Pattern
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities



multiMatchAndReplace :: [ (Expr, Expr) ] -> Expr -> Expr
multiMatchAndReplace prs subj = foldr matchAndReplace subj prs


matchAndReplace :: (Expr, Expr) -> Expr -> Expr
matchAndReplace (pat, repl) subj =
  let
    minfos = matches pat subj
  in
   canonicalize $ replaceAll minfos repl subj
    

replaceAll :: [ MatchInfo ] -> Expr -> Expr -> Expr
replaceAll minfos repl subj =
  let
    subj' = deleteMatches minfos subj
    
    replace' minfo s = let
      repl' = unPattern minfo repl
      bcs   = matchPaths minfo
      in
       replaceIn bcs repl' s
  in
   foldr replace' subj' minfos
   
   
-- Remove the items at the paths specified in minfo,
-- the use the environment to insert the expression 
-- repl into the to expression subj.
--
replace :: MatchInfo -> Expr -> Expr -> Expr
replace minfo repl subj =
  let
    repl' = unPattern minfo repl
    subj' = deleteMatches [minfo] subj
    bcs   = matchPaths minfo
  in
   replaceIn bcs repl' subj'
   
   
replaceIn :: [ Breadcrumbs' ] -> Expr -> Expr -> Expr
replaceIn bcs repl subj =
  let
    subjSpaces = zip (map (\bc -> repSpaces $ exprAt bc subj) bcs) bcs
    replSpaces = repSpaces repl
  in
   if null replSpaces
   then replaceAt (head bcs) repl subj
   else if length replSpaces == 1
        then let
          bc = lookup replSpaces subjSpaces
          in if isJust bc
             then replaceAt (fromJust bc) repl subj
             else error "replaceIn: no matching repSpace"
        else error "replaceIn: replacement expression must be in single repSpace."


-- Take an expression an replace all of the patterns
-- with their values from the match environment.
--
-- XXX FIXME XXX
-- Doesn't replace all of the patterns.
--
unPattern :: MatchInfo -> Expr -> Expr
unPattern minfo ex =
  let
    env = matchEnviron minfo
  in
   mapExpr (replacePattern env) ex


replacePattern :: (Map.Map PatternVar Binding) -> Expr -> Expr
replacePattern env e@(Symbol (Tensor t)) =
  if isPattern e
  then
     let
       repl = Map.lookup (getPattern e) env
     in
      if isJust repl
      then let
        TVar t' = fromJust repl
        s'      = map (replaceSlot env) (slots t)
        in Symbol $ Tensor $ t' { slots = s'}
      else error "replacePattern pattern without binding"
  else
    let
      s' = map (replaceSlot env) (slots t)
    in Symbol $ Tensor $ t { slots = s' }

replacePattern env e = if isPattern e
                       then let repl = Map.lookup (getPattern e) env
                            in if isJust repl
                               then unBind $ fromJust repl
                               else error "replacePattern: pattern without binding"
                       else e
                            
                            
replaceSlot :: (Map.Map PatternVar Binding) -> VarIndex -> VarIndex
replaceSlot env s = if isPatternVarIndex s
                    then
                      let
                        s' = Map.lookup (getVarIndexPattern s) env
                      in if isJust s'
                         then if isCovariant s
                              then Covariant     (Abstract (unBindIndex $ fromJust s'))
                              else Contravariant (Abstract (unBindIndex $ fromJust s'))
                         else error "replaceSlot: pattern without binding"
                    else s
                                             
                         
deleteMatches :: [ MatchInfo ] -> Expr -> Expr
deleteMatches minfos ex = foldr (\m e -> deleteExprs (matchPaths m) e) ex minfos


-- A convenience function to delete several subexpressions
-- from an expression tree.
--
deleteExprs :: [ Breadcrumbs' ] -> Expr -> Expr
deleteExprs bcs e = foldr deleteExpr e bcs


replaceInContext :: Breadcrumbs' -> Expr -> Expr
replaceInContext _ e = 
  let
    rs = repSpaces e
  in
   if null rs
   then Symbol $ mkPlaceholder
   else Symbol $ mkNcPlaceholder (head rs)


-- deleteExpr removes the expression subtree at the
-- specified location.
--
deleteExpr :: Breadcrumbs' -> Expr -> Expr
deleteExpr bc e = snd $ de bc (DList.empty, e)
  where
    de targetLoc (currentLoc, expr) = if currentLoc == targetLoc
                                      then (currentLoc, replaceInContext currentLoc expr)
                                      else de' targetLoc (currentLoc, expr)
                                           
    de' targetLoc (currentLoc, Sum ts)     = (currentLoc, Sum     (zipWith (\n x -> snd (de targetLoc (currentLoc `DList.snoc` Scxt n, x))) [1..] ts))
    de' targetLoc (currentLoc, Product fs) = (currentLoc, Product (zipWith (\n x -> snd (de targetLoc (currentLoc `DList.snoc` Pcxt n, x))) [1..] fs))
    de' _ u@(_, _) = u
    
    
-- A limited expression replacement function: replaceExpr
-- replaces the subtree at the specified location with
-- a new expression subtree.
--
replaceExpr :: Breadcrumbs -> Expr -> Expr -> Expr
replaceExpr bc repl subj = snd $ re bc repl ([], subj)
    where
      re targetLoc rexpr (currentLoc, expr) = if currentLoc == targetLoc
                                                  then (currentLoc, rexpr)
                                                  else re' targetLoc rexpr (currentLoc, expr)
                                                       
      re' targetLoc rexpr (currentLoc, Sum ts)     = (currentLoc, Sum     (zipWith (\n x -> snd (re targetLoc rexpr ((Scxt n) : currentLoc, x))) [1..] ts))
      re' targetLoc rexpr (currentLoc, Product fs) = (currentLoc, Product (zipWith (\n x -> snd (re targetLoc rexpr ((Pcxt n) : currentLoc, x))) [1..] fs))
      re' _ _ u@(_, _) = u
  

-- A utility funtion to return the subexpression at a
-- given location in the expression tree.
--
exprAt :: Breadcrumbs' -> Expr -> Expr
exprAt bc e = exat (DList.toList bc) e
  where
    exat [] ex = ex
    exat ((Scxt n) : bcs) (Sum ts)     = exat bcs (ts !! (n - 1))
    exat ((Pcxt n) : bcs) (Product fs) = exat bcs (fs !! (n - 1))
    exat _ _ = error "exprAt: incompatible Expr and Breadcrumbs"
    
    
-- A utility function to insert a given expression at a specified location.
-- No different than replaceExpr, but the location is given as
-- Breadcrumbs' instead of Breadcrumbs.  (This really ought to be
-- fixed, as there is no need for two ways of doing this.)
--
replaceAt :: Breadcrumbs' -> Expr -> Expr -> Expr
replaceAt bc repl subj = snd $ re bc repl (DList.empty, subj)
  where
    re targetLoc rexpr (currentLoc, expr) = if currentLoc == targetLoc
                                            then (currentLoc, rexpr)
                                            else re' targetLoc rexpr (currentLoc, expr)
                                                       
    re' targetLoc rexpr (currentLoc, Sum ts)     = (currentLoc, Sum     (zipWith (\n x -> snd (re targetLoc rexpr (currentLoc `DList.snoc` Scxt n, x))) [1..] ts))
    re' targetLoc rexpr (currentLoc, Product fs) = (currentLoc, Product (zipWith (\n x -> snd (re targetLoc rexpr (currentLoc `DList.snoc` Pcxt n, x))) [1..] fs))
    re' _ _ u@(_, _) = u
