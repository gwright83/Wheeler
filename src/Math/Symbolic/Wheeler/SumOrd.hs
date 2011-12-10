--
-- SumOrd.hs
--
-- Class for comparing noncommuting objects in sums.
--
-- Gregory Wright, 30 August 2011
--

module Math.Symbolic.Wheeler.SumOrd where

-- We need a special comparison operation for Sums.
-- Symbols not commuting under the Product operation
-- commute under a Sum and we can use "sumCompare"
-- to put them in canonical order.

class Eq a => SumOrd a where
    sumCompare :: a -> a -> Ordering

