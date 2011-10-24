-- Copyright (c) 2011 Kevin Cantu

module HilbertCombo (hrtFromCoordList, hrtSearchWithCoord, HilbertRTree) where

import Data.List
import HilbertCoordinates  -- lab2
import HilbertCurve        -- lab2
import HilbertRTree        -- lab2



-- read the coordinates as an array
readPoints :: String -> [Pt]
readPoints coords =
   let
      tup (xy, r) = (Pt (xy !! 0) (xy !! 1), r)

      flatArray = read ("[" ++ coords ++ "]") :: [Integer]

      f [] = Nothing
      f xs = case length xs `mod` 2 of
               0 -> Just (tup $ splitAt 2 xs)
               _ -> error "this object path is junk"
   in
      if length flatArray == 8
      then unfoldr f flatArray
      else error "expected 4 points, e.g.: x0,y0,x1,y1,x2,y2,x3,y3"


-- function to calculate bounding rectangle
-- of many points
pointsToRect :: [Pt] -> MBR
pointsToRect []       = error "there is no bounding rectangle of an empty coordinate"
pointsToRect cs@(c:_) =
   let
      toRect (Pt x y) = MBR (Pt x y) (Pt x y)

      maxR (MBR (Pt minx miny) (Pt maxx maxy)) (Pt x y) =
         let
            minx' = min minx x
            miny' = min miny y
            maxx' = max maxx x
            maxy' = max maxy y
         in 
            MBR (Pt minx' miny') (Pt maxx' maxy')
   in
      foldl maxR (toRect c) cs


coordsToRect :: String -> MBR
coordsToRect =
   pointsToRect . readPoints


coordsToInfo :: String -> Tree
coordsToInfo coords =
   let
      rect = coordsToRect coords

      center :: MBR -> Pt
      center (MBR (Pt x y) (Pt x' y')) = Pt (div (x+x') 2) (div (y+y') 2)

      h = LHV $ hilbertValue $ center rect
   in
      Info rect h coords
                           

hrtFromCoordList :: [String] -> HilbertRTree
hrtFromCoordList rawStrings =
   let
      -- since we start with a list of strings
      -- turn each into an Info, then insert
      insertCoords = insertHRT . coordsToInfo
   in
      foldl (flip insertCoords) NewHRT rawStrings


hrtSearchWithCoord :: String -> HilbertRTree -> [String]
hrtSearchWithCoord queryString hrt =
   let
      -- since searchHRT returns a list of Info
      -- pull the strings out
      getStr (Info _ _ str) = str
      getStr _ = error "what did we return, a Branch or Leaf?!"
   in
      map getStr $ searchHRT (coordsToRect queryString) hrt
  
   
