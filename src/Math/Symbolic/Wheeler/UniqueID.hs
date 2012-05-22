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
    nextId,
    nextDummy
) where


import Data.Unique.Id


class Identified a where
    identifier :: a -> Id


-- Initialize the supply of unique IDs:
--
identifiers :: IO IdSupply
identifiers = initIdSupply 's'


-- Produce the next identifier:
--
nextId :: IO Id
nextId = do
    return . idFromSupply =<< identifiers


dummyIdentifiers :: IO IdSupply
dummyIdentifiers = initIdSupply 'd'


-- Produce the next dummy identifier:
--
nextDummy :: IO Id
nextDummy = do
    return . idFromSupply =<< dummyIdentifiers


