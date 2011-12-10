--
-- Precedence.hs
--
-- Precedence defintions for Show instances.
--
-- Gregory Wright, 27 August 2011
--

module Math.Symbolic.Wheeler.Precedence where


-- For the showInternal function we only care if the
-- precedence is more or less than list construction:

listPrecedence :: Int
listPrecedence    = 1

nonListPrecedence :: Int
nonListPrecedence = 2

-- Precedence for the standard Show instances of parts
-- of an expression:

applicationPrecedence :: Int
applicationPrecedence = 10

powerPrecedence :: Int
powerPrecedence       = 8

productPrecedence :: Int
productPrecedence     = 7

sumPrecedence :: Int
sumPrecedence         = 6


