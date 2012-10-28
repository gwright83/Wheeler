--
-- Debug.hs
--
-- Display functions calls, arguments and results.
--
-- Gregory Wright, 12 June 2011
--

module Math.Symbolic.Wheeler.Debug where

import Debug.Trace


traceCall :: (Show a, Show b) => String -> (a -> b) -> a -> b
traceCall msg fn arg =
    let
        retval = fn arg
    in 
        trace (msg         ++
               " "         ++
               show arg    ++
               " returns " ++
               show retval ++
               "\n") retval

traceCall2 :: (Show a, Show b, Show c) => String -> (a -> b -> c) -> a -> b -> c
traceCall2 msg fn a a' =
    let
        retval = fn a a'
    in 
        trace (msg         ++
               " "         ++
               show a      ++
               " "         ++
               show a'     ++
               " returns " ++
               show retval ++
               "\n") retval

traceCall3 :: (Show a, Show b, Show c) => String -> a -> b -> c -> c
traceCall3 msg a a' r =
    let
        retval = r
    in 
        trace (msg         ++
               " "         ++
               show a      ++
               " "         ++
               show a'     ++
               " returns " ++
               show retval ++
               "\n") retval

traceCall3' :: (Show a, Show b, Show c, Show d) => String -> (a -> b -> c -> d) -> a -> b -> c -> d
traceCall3' msg fn a a' a'' =
    let
        retval = fn a a' a''
    in 
        trace (msg         ++
               " "         ++
               show a      ++
               " "         ++
               show a'     ++
               " "         ++
               show a''    ++
               " returns " ++
               show retval ++
               "\n") retval


traceCall_ :: (Show a, Show b) => String -> a -> b -> b
traceCall_ msg arg val = trace (msg ++ " " ++ show arg ++ " returns " ++ show val ++ "\n") val