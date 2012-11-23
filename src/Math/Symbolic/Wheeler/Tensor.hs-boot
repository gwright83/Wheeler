--
-- Tensor.hs-boot
--


module Math.Symbolic.Wheeler.Tensor where

import Math.Symbolic.Wheeler.Commutativity
import Math.Symbolic.Wheeler.Named
import Math.Symbolic.Wheeler.UniqueID


data T

instance Eq T
instance Ord T
instance Named T
instance Identified T
instance Show T
instance Commutable T

