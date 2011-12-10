--
-- Annotation.hs
--
-- Annotation of the expression tree.
--
-- Gregory Wright, 8 November 2011
--

module Math.Symbolic.Wheeler.Annotation where


class Functor expr => Annotated expr where
    ann :: expr a -> a
    amap :: (a -> a) -> expr a -> expr a

