-- Copyright (c) 2011 Kevin Cantu

module HilbertCoordinates (Pt(..), MBR(..), LHV(..)) where

data Pt  = Pt  Integer Integer deriving (Show, Ord, Eq)
data MBR = MBR Pt Pt           deriving (Show, Ord, Eq)
data LHV = LHV Integer         deriving (Show, Ord, Eq)


