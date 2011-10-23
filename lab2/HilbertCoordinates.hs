-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental

module HilbertCoordinates (Pt(..), MBR(..), LHV(..), H, R, r0, h0) where

data Pt  = Pt  Integer Integer deriving (Show, Ord, Eq)
data MBR = MBR Pt Pt           deriving (Show, Ord, Eq)
data LHV = LHV Integer         deriving (Show, Ord, Eq)

-- rename these three later
type R = MBR
type H = LHV

-- convenient null values
r0 :: MBR
r0 = MBR (Pt 0 0) (Pt 0 0)
h0 :: LHV
h0 = LHV 0


