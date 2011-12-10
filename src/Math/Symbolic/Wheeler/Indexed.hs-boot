--
-- Indexed.hs-boot
--
-- Break import cycle
--

module Math.Symbolic.Wheeler.Indexed where

import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.UniqueID

data I

instance Commutable I
instance Eq I
instance Identified I
instance Named I
instance Ord I
instance Show I
