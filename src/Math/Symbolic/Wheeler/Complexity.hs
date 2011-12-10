--
-- Complexity.hs
--
-- Definition of complex quantities and
-- the operations allowed on them.
--
-- Gregory Wright, 26 August 2011
--

module Math.Symbolic.Wheeler.Complexity (
    Complexity (..)
) where

data Complexity = Real
                | Imaginary
                | Complex
                | Hermitian
                | AntiHermitian
                deriving (Eq, Show)

