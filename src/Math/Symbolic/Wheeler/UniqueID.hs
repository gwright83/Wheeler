--
-- UniqueID.hs
--
-- A source of unique identifiers
--
-- Gregory Wright, 1 September 2011
--


module Math.Symbolic.Wheeler.UniqueID (
    Id,
    Identified (..),
    nextId
) where


import Data.Unique.Id


class Identified a where
    identifier :: a -> Id


-- initialize the supply of unique IDs:
--
identifiers :: IO IdSupply
identifiers = initIdSupply 's'


-- produce the next identifier:
--
nextId :: IO Id
nextId = do
    return . idFromSupply =<< identifiers
