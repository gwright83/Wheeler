--
-- Named.hs
--
-- Class for named objects that have an ordinary
-- name and a possibly distinct TeX name.
-- 
-- The usual way to use this is to declare records
-- as instances of the class Named, with the "name"
-- function equal to the selector for the name field.
--
-- Gregory Wright, 16 August 2011
--

module Math.Symbolic.Wheeler.Named (
       Named (..)
) where


class Named a where
    name    :: a -> String
    teXName :: a -> String
