-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental

module HilbertCurve (Rect(..), Pt(..), hilbert) where

import HilbertCoordinates

-- the hilbert curve fractal behaves differently
-- for each of the four vertexes
data Quadrant = NW | NE  --   +---+
              | SW | SE  --   |   |
                         --   +   +
              deriving (Eq, Show)

quad2num :: Num a => Quadrant -> a
quad2num q = case q of
               SW -> 0
               NW -> 1
               NE -> 2
               SE -> 3

data Rect = Rect Pt Pt deriving (Eq, Ord, Show)

-- find which quadrant this point is in
quadrant :: Rect -> Pt -> Quadrant
quadrant (Rect (Pt ax ay) (Pt cx cy)) (Pt x y) =
   let
      topHalf a b c = abs (b - a) > abs (c - b)
   in
      case (topHalf ax x cx, topHalf ay y cy) of
         (False, False) -> SW
         (False, True)  -> NW
         (True,  True)  -> NE
         (True,  False) -> SE

-- rotate to get the new lower-left and upper-right corners
-- of this quadrant
rotateCorners :: Rect -> Quadrant -> Rect
rotateCorners (Rect (Pt ax ay) (Pt cx cy)) q = 
   let
      avg i j = (i + j) `div` 2
      bx = avg ax cx
      by = avg ay cy
   in
      case q of
         SW -> Rect (Pt by ax) (Pt ay bx) -- rotate clockwise 90 
         NW -> Rect (Pt ax by) (Pt bx cy) -- don't rotate
         NE -> Rect (Pt bx by) (Pt cx cy) -- don't rotate
         SE -> Rect (Pt ay cx) (Pt by bx) -- rotate ccw 90

-- rotate the point x,y if necessary
rotatePoint :: Pt -> Quadrant -> Pt
rotatePoint (Pt x y) q =
   case q of
      SW -> Pt y x
      NW -> Pt x y
      NE -> Pt x y
      SE -> Pt y x

-- Given x,y coordinates, we can calculate a hilbert curve position
-- by stepping through the fractal to a desired resolution.
-- At each step, the quadrant in which we find the point
-- is rotated and split again.
hilbert' :: Integer  -- desired order of hilbert curve
         -> Integer  -- current order
         -> Integer  -- position at previous order
         -> Rect     -- corners of previous quadrant
         -> Pt       -- actual coordinate
         -> Integer  -- hilbert position
hilbert' targetOrder order prevHilbert r p =
   -- TODO: need to bounds check the point...
   let
      q  = quadrant r p         -- find where x,y is
      r' = rotateCorners r q    -- get corners of that quadrant
      p' = rotatePoint p q      -- maybe rotate the point
      h  = prevHilbert * 4 + quad2num q  -- find our new hilbert location
   in
      -- have we gone far enough?
      if order >= targetOrder
      then h
      else hilbert' targetOrder (order + 1) h r' p'

-- find a point's location on hilbert curve
hilbert :: Pt -> Integer
hilbert p = hilbert' hilbertOrder initialOrder hilbert0 (Rect a c) p
          where
            initialOrder = 1
            hilbert0     = 0
            a            = Pt 0 0
            c            = Pt 65536 65536
            hilbertOrder = 4  -- total quadrants: 4 ^ hilbertOrder

