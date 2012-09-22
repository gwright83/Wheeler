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

import Math.Symbolic.Wheeler.Common
import Math.Symbolic.Wheeler.Expr
import Math.Symbolic.Wheeler.Matchable
import Math.Symbolic.Wheeler.Matcher2
import Math.Symbolic.Wheeler.Numeric
import Math.Symbolic.Wheeler.Pattern
import Math.Symbolic.Wheeler.Symbol
import Math.Symbolic.Wheeler.Tensor
import Math.Symbolic.Wheeler.TensorUtilities


-- Remove the items at the paths specified in minfo,
-- the use the environment to insert the expression 
-- repl into the to expression subj.
--
--replace :: MatchInfo -> Expr -> Expr -> Expr
--replace minfo repl subj


-- Take an expression an replace all of the patterns
-- with their values from the match environment.
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
      else error "replacePattern patern without binding"
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


replaceInContext :: Breadcrumbs' -> Expr
replaceInContext bc = if bc == DList.empty || isScxt (DList.head bc)
                      then (Const (I 0))
                      else (Const (I 1))


-- deleteExpr removes the expression subtree at the
-- specified location.
--
deleteExpr :: Breadcrumbs' -> Expr -> Expr
deleteExpr bc e = snd $ de bc (DList.empty, e)
  where
    de targetLoc (currentLoc, expr) = if currentLoc == targetLoc
                                      then (currentLoc, replaceInContext currentLoc)
                                      else de' targetLoc (currentLoc, expr)
                                           
    de' targetLoc (currentLoc, Sum ts)     = (currentLoc, Sum     (zipWith (\n x -> snd (de targetLoc (currentLoc `DList.snoc` Scxt n, x))) [1..] ts))
    de' targetLoc (currentLoc, Product fs) = (currentLoc, Product (zipWith (\n x -> snd (de targetLoc (currentLoc `DList.snoc` Pcxt n, x))) [1..] fs))
    de' _ u@(_, _) = u
    
    
-- A limited expression replacement function: replaceExpr
-- replaces the subtree at the specified location with
-- a new expression subtree.
--
replaceExpr :: Breadcrumbs -> Expr -> Expr -> Expr
replaceExpr bc e' e = snd $ re bc e' ([], e)
    where
      re targetLoc rexpr (currentLoc, expr) = if currentLoc == targetLoc
                                                  then (currentLoc, rexpr)
                                                  else re' targetLoc rexpr (currentLoc, expr)
                                                       
      re' targetLoc rexpr (currentLoc, Sum ts)     = (currentLoc, Sum     (zipWith (\n x -> snd (re targetLoc rexpr ((Scxt n) : currentLoc, x))) [1..] ts))
      re' targetLoc rexpr (currentLoc, Product fs) = (currentLoc, Product (zipWith (\n x -> snd (re targetLoc rexpr ((Pcxt n) : currentLoc, x))) [1..] fs))
      re' _ _ u@(_, _) = u
  
