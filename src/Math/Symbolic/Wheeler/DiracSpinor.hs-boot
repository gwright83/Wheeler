--
-- DiracSpinor.hs-boot
--


module Math.Symbolic.Wheeler.DiracSpinor where

import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.UniqueID

data D

instance Eq D
instance Ord D
instance Named D
instance Identified D
instance Show D
instance Commutable D