-- Copyright (c) 2011 Kevin Cantu

module HilbertCoordinates (Pt(..), Rect(..), LHV(..)) where

data Pt   = Pt  Integer Integer deriving (Show, Ord, Eq)
data Rect = Rect Pt Pt          deriving (Show, Ord, Eq)
data LHV  = LHV Integer         deriving (Show, Ord, Eq)


