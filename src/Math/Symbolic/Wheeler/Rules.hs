--
-- Rules.hs
--
-- Top level module for pattern matching and replacement.
--
-- Gregory Wright, 13 September 2012
--

module Math.Symbolic.Wheeler.Rules where


import Math.Symbolic.Wheeler.Expr


type Rule = Expr -> Expr