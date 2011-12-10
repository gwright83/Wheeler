--
-- Symbol.hs-boot
--
-- Break import cycle for Symbol
--
-- Gregory Wright, 29 August 2011
--

module Math.Symbolic.Wheeler.Symbol where

import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Named

data Symbol

instance Commutable Symbol
instance Named Symbol
instance Ord Symbol
