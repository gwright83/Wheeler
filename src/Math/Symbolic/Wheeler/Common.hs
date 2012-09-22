{-# LANGUAGE TypeSynonymInstances, 
             FlexibleInstances #-}
--
-- Common.hs
--
-- Some common definitions related to navigating
-- expression trees.
--
-- Gregory Wright, 2 July 2012
--

module Math.Symbolic.Wheeler.Common where


import Data.DList


data Cxt = Scxt Int | Pcxt Int | Tcxt Int
     deriving (Eq, Ord, Show)

isScxt :: Cxt -> Bool
isScxt (Scxt _) = True
isScxt _        = False

isPcxt :: Cxt -> Bool
isPcxt (Pcxt _) = True
isPcxt _        = False

isTcxt :: Cxt -> Bool
isTcxt (Tcxt _) = True
isTcxt _        = False

type Breadcrumbs = [ Cxt ]
type Breadcrumbs' = DList Cxt

instance Eq Breadcrumbs' where
  (==) x y = (==) (toList x) (toList y)

instance Show Breadcrumbs' where
  show = show . toList

data SymbolType = Regular
                | Pattern
                deriving (Eq, Show)