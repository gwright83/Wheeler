-- Function.hs
--
-- The Function datatype; the base module imported by anything
-- that operates on symbolic functions. 
--
-- Gregory Wright, 3 June 2011.
--
-- Copyright (c) 2011, Antiope Associates, all rights reserved.
--

module Math.Symbolic.Wheeler.Function (
       Function (..),
) where


import Math.Symbolic.Wheeler.Precedence


data Function = Id
              | Abs
              | Signum
              | Exp
              | Log
              | Sin
              | Cos
              | Tan
              | Asin
              | Acos
              | Atan
              | Sinh
              | Cosh
              | Tanh
              | Asinh
              | Acosh
              | Atanh
              | Sqrt
              | Func String
              deriving (Eq, Ord)


instance Show Function where
    showsPrec d (Func f) = showParen (d > listPrecedence) $ showString "Func " .
                                                            showString "\""    .
                                                            showString f       .
                                                            showString "\""
    showsPrec _ Id       = showString "id"
    showsPrec _ Abs      = showString "abs"
    showsPrec _ Signum   = showString "signum"
    showsPrec _ Exp      = showString "exp"
    showsPrec _ Log      = showString "log"
    showsPrec _ Sin      = showString "sin"
    showsPrec _ Cos      = showString "cos"
    showsPrec _ Tan      = showString "tan"
    showsPrec _ Asin     = showString "asin"
    showsPrec _ Acos     = showString "acos"
    showsPrec _ Atan     = showString "atan"
    showsPrec _ Sinh     = showString "sinh"
    showsPrec _ Cosh     = showString "cosh"
    showsPrec _ Tanh     = showString "tanh"
    showsPrec _ Asinh    = showString "asinh"
    showsPrec _ Acosh    = showString "acosh"
    showsPrec _ Atanh    = showString "atanh"
    showsPrec _ Sqrt     = showString "sqrt"

