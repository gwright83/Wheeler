--
-- Commutativity.hs
--
-- Definitions and operations for non-commuting objects.
--
-- Gregory Wright, 26 August 2011
--

module Math.Symbolic.Wheeler.Commutativity (
    Commutable (..),
    Commutativity (..),
    RepSpace (..),
    repSpace,
    isCommuting,
    isNonCommuting
) where


class Commutable a where
    commutativity :: a -> Commutativity

data Commutativity = Commuting | NonCommuting RepSpace
     deriving (Eq, Show)

newtype RepSpace = RepSpace String
     deriving (Eq, Show)

repSpace :: (Commutable a) => a -> Maybe String
repSpace x = rep (commutativity x)
     where
         rep :: Commutativity -> Maybe String
         rep Commuting                   = Nothing
         rep (NonCommuting (RepSpace s)) = Just s

isCommuting :: (Commutable a) => a -> Bool
isCommuting = not . isNonCommuting

isNonCommuting :: (Commutable a) => a -> Bool
isNonCommuting x = nc (commutativity x)
     where
         nc :: Commutativity -> Bool
         nc (NonCommuting (RepSpace _)) = True
         nc Commuting                   = False
         
